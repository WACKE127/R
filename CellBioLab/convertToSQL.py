#!/usr/bin/env python
import csv
import sqlite3

# -------------------------------------------------------------------------
# 1) Base densities per week
# -------------------------------------------------------------------------
BASE_DENSITY_WEEK1 = 2.79e6
BASE_DENSITY_WEEK2 = 4.50e6

def get_density(week, condition):
    """Return absolute cell density for the given week & condition."""
    cond = condition.strip().upper()
    if week == 1:
        if cond == "4C":
            return 4.0 * BASE_DENSITY_WEEK1
        elif cond == "2C":
            return 2.0 * BASE_DENSITY_WEEK1
        elif cond == "C":
            return 1.0 * BASE_DENSITY_WEEK1
        elif cond == "1/2C":
            return 0.5 * BASE_DENSITY_WEEK1
        elif cond == "NDF":
            return BASE_DENSITY_WEEK1
        else:
            return None
    else:  # week == 2
        if cond == "4C":
            return 4.0 * BASE_DENSITY_WEEK2
        elif cond == "2C":
            return 2.0 * BASE_DENSITY_WEEK2
        elif cond == "C":
            return 1.0 * BASE_DENSITY_WEEK2
        elif cond == "1/2C":
            return 0.5 * BASE_DENSITY_WEEK2
        elif cond == "NDF":
            return BASE_DENSITY_WEEK2
        else:
            return None

# -------------------------------------------------------------------------
# 2) Researcher assignment
#    - Week 1: 2C, C => R1; 4C, 1/2C, NDF => R2
#    - Week 2: 4C, 1/2C, NDF => R1; 2C, C => R2
# -------------------------------------------------------------------------
def get_researcher(week, condition):
    cond = condition.strip().upper()
    if cond == "NDF":
        # NDF: week1->R2, week2->R1
        return 2 if week == 1 else 1
    if week == 1:
        if cond in ("2C","C"):
            return 1
        else:
            return 2
    else:  # week == 2
        if cond in ("4C","1/2C"):
            return 1
        else:
            return 2

# -------------------------------------------------------------------------
# 3) Block definitions
# -------------------------------------------------------------------------
week1_blocks = [
    ("2C",   "4C",   9),  # times=0..90 in steps (9 lines)
    ("C",    "1/2C", 9),
    ("NDF",  None,   2)   # times=0,90 only
]
week2_blocks = [
    ("4C",   "2C",   9),
    ("1/2C", "C",    9),
    ("NDF",  None,   2)
]

# -------------------------------------------------------------------------
# 4) parse_side attempts to parse time and replicates from a row's columns
#    columns[start_col] => time
#    columns[start_col+1..+10] => replicate lengths
#    Returns None if time is blank or cannot be parsed
# -------------------------------------------------------------------------
def parse_side(columns, start_col):
    if start_col >= len(columns):
        return None
    time_str = columns[start_col].strip()
    if not time_str:
        return None
    # Try to parse the time as float
    try:
        time_min = float(time_str)
    except ValueError:
        return None

    replicate_lengths = []
    for i in range(1, 11):
        col_idx = start_col + i
        val = None
        if col_idx < len(columns):
            cell_str = columns[col_idx].strip()
            if cell_str:
                try:
                    val = float(cell_str)
                except ValueError:
                    val = None
        replicate_lengths.append(val)

    return (time_min, replicate_lengths)

# -------------------------------------------------------------------------
# 5) read_data_lines(reader, n_data): read exactly n_data *valid* lines
#    A "valid" line is one where parse_side returns non-None for left and/or right
# -------------------------------------------------------------------------
def read_data_lines(reader, cond_left, cond_right, n_data):
    """Reads the CSV until we've parsed n_data valid lines (ignoring blank or invalid lines).
       Returns a list of (left_data, right_data) tuples of length n_data."""
    block_rows = []
    lines_collected = 0

    while lines_collected < n_data:
        try:
            row = next(reader)
        except StopIteration:
            break

        # If row is completely blank, skip
        if not any(cell.strip() for cell in row):
            continue

        # parse left & right
        left_data = parse_side(row, 0) if cond_left else None
        right_data = parse_side(row, 13) if cond_right else None

        # If neither side yields a valid time, skip
        if not left_data and not right_data:
            continue

        block_rows.append((left_data, right_data))
        lines_collected += 1

    return block_rows

# -------------------------------------------------------------------------
# 6) Insert a single row of data into the DB
# -------------------------------------------------------------------------
def insert_data(week_num, condition, time_min, replicate_lengths, cur):
    """Compute researcher & density, then insert replicate rows."""
    researcher = get_researcher(week_num, condition)
    density_val = get_density(week_num, condition)

    for i, length_val in enumerate(replicate_lengths, start=1):
        cur.execute("""
            INSERT INTO flagella_measurements
                (week, researcher, condition, density, time_min, replicate, length_um)
            VALUES (?,?,?,?,?,?,?)
        """, (week_num, researcher, condition, density_val,
              time_min, i, length_val))

# -------------------------------------------------------------------------
# 7) parse_week: for each (cond_left, cond_right, n_data), read n_data lines
#    then do the inserts for left & right data if present
# -------------------------------------------------------------------------
def parse_week(week_num, blocks, reader, cur):
    for (cond_left, cond_right, n_lines) in blocks:
        # read n_lines valid data lines
        block_data = read_data_lines(reader, cond_left, cond_right, n_lines)
        if len(block_data) < n_lines:
            # We didn't find enough lines to fill this block;
            # Could raise an error, or just break
            print(f"WARNING: Found only {len(block_data)} lines instead of {n_lines} for block ({cond_left}, {cond_right}) in Week {week_num}.")
        # Insert
        for (left_d, right_d) in block_data:
            if left_d:
                tmin, repls = left_d
                insert_data(week_num, cond_left, tmin, repls, cur)
            if right_d:
                tmin, repls = right_d
                insert_data(week_num, cond_right, tmin, repls, cur)

# -------------------------------------------------------------------------
# 8) main: set up DB, read CSV, parse blocks
# -------------------------------------------------------------------------
def main():
    conn = sqlite3.connect("flagella_data.sqlite")
    cur = conn.cursor()

    # Drop old table if it exists
    cur.execute("DROP TABLE IF EXISTS flagella_measurements")
    cur.execute("""
        CREATE TABLE flagella_measurements (
            measurement_id INTEGER PRIMARY KEY AUTOINCREMENT,
            week           INTEGER NOT NULL,
            researcher     INTEGER,
            condition      TEXT NOT NULL,
            density        REAL,
            time_min       REAL NOT NULL,
            replicate      INTEGER NOT NULL,
            length_um      REAL
        )
    """)

    with open("flagella_data.csv", "r", newline="", encoding="utf-8") as f:
        reader = csv.reader(f)

        # Skip lines until we see "Week One Flagella" in uppercase
        for row in reader:
            if "WEEK ONE FLAGELLA" in ",".join(cell.upper() for cell in row):
                break

        parse_week(1, week1_blocks, reader, cur)

        # skip lines until we see "Week Two Flagella"
        for row in reader:
            if "WEEK TWO FLAGELLA" in ",".join(cell.upper() for cell in row):
                break

        parse_week(2, week2_blocks, reader, cur)

    conn.commit()
    conn.close()
    print("Done! Created flagella_data.sqlite with corrected NDF times.")

# -------------------------------------------------------------------------
if __name__ == "__main__":
    main()
