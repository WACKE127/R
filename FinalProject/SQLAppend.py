import requests
import psycopg2
import os
from dotenv import load_dotenv

def create_table(conn):
    with conn.cursor() as cur:
        cur.execute("""
            CREATE TABLE IF NOT EXISTS items (
                item_id TEXT PRIMARY KEY,
                item_name TEXT,
                listing_title TEXT,
                seller TEXT,
                msrp TEXT,
                listed_price TEXT,
                movement TEXT,
                brand TEXT,
                condition TEXT,
                department TEXT,
                sales TEXT
            )
        """)
    conn.commit()

def fetch_watches(token, query="watches", limit=10):
    url = "https://api.ebay.com/buy/browse/v1/item_summary/search"
    headers = {"Authorization": f"Bearer {token}"}
    params = {"q": query, "limit": limit}
    r = requests.get(url, headers=headers, params=params)
    return r.json().get("itemSummaries", [])

def insert_data(conn, items):
    with conn.cursor() as cur:
        for i in items:
            cur.execute("""
                INSERT INTO items (
                    item_id,
                    item_name,
                    listing_title,
                    seller,
                    msrp,
                    listed_price,
                    movement,
                    brand,
                    condition,
                    department,
                    sales
                ) VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)
                ON CONFLICT (item_id) DO NOTHING
            """, (
                i.get("itemId"),
                i.get("title"),
                i.get("title"),
                i.get("seller", {}).get("username"),
                i.get("price", {}).get("value"),
                i.get("price", {}).get("value"),
                i.get("additionalProductIdentities", [{}])[0].get("productAspect", {}).get("movement"),
                i.get("brand"),
                i.get("condition"),
                i.get("department"),
                None
            ))
    conn.commit()

def main():
    load_dotenv("APIs.ENV")
    token = os.getenv("eBay")  # Reads the key from the ENV file
    
    conn = psycopg2.connect(
        dbname="eBayProject",
        user="project",
        password="Leswhdc9*",
        host="/var/run/postgresql",
        port="5432"
    )

    create_table(conn)
    items = fetch_watches(token)
    insert_data(conn, items)
    conn.close()

if __name__ == "__main__":
    main()
