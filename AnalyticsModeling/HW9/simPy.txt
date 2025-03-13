import random
import statistics
import simpy
import matplotlib.pyplot as plt
from scipy.stats import poisson

from shiny import App, ui, reactive, render

import matplotlib
matplotlib.use("Agg")


def run_simulation(num_id_checkers, num_personal_checks, sim_time=100, arrival_rate=5):
    """
    Run a SimPy simulation of the airport security system.
    
    Passengers arrive following a Poisson process with rate 'arrival_rate' (per minute).
    They first wait for the ID/boarding-pass check (with exponential service time, mean 0.75 min)
    and then for the personal check (with uniformly distributed service time between 0.5 and 1 min).
    
    We record:
      - The total time in the system.
      - The wait time (queue delay) for the ID check.
      - The wait time for the personal check.
    
    Returns:
      avg_total: Average total time in system per passenger.
      avg_id: Average waiting time for ID/boarding-pass check.
      avg_personal: Average waiting time for personal check.
      n_passengers: Total number of passengers processed.
    """
    env = simpy.Environment()
    id_resource = simpy.Resource(env, capacity=num_id_checkers)
    personal_resources = [simpy.Resource(env, capacity=1) for _ in range(num_personal_checks)]
    
    waiting_times_total = []
    id_wait_times = []
    personal_wait_times = []
    
    def passenger(env, name):
        arrival_time = env.now
        
        # Stage 1: ID/boarding-pass check
        with id_resource.request() as req:
            yield req
            time_id_start = env.now
            id_wait = time_id_start - arrival_time
            id_wait_times.append(id_wait)
            service_time = random.expovariate(1 / 0.75)  # mean 0.75 minutes
            yield env.timeout(service_time)
        
        # Stage 2: Personal check
        time_personal_request = env.now
        # Choose the personal-check resource with the shortest queue.
        chosen_resource = min(personal_resources, key=lambda r: len(r.queue))
        with chosen_resource.request() as req:
            yield req
            time_personal_start = env.now
            personal_wait = time_personal_start - time_personal_request
            personal_wait_times.append(personal_wait)
            personal_service_time = random.uniform(0.5, 1.0)
            yield env.timeout(personal_service_time)
        
        departure_time = env.now
        total_time = departure_time - arrival_time
        waiting_times_total.append(total_time)
    
    def passenger_generator(env):
        i = 0
        while env.now < sim_time:
            i += 1
            env.process(passenger(env, f"Passenger {i}"))
            # Interarrival time: exponential with mean 1/arrival_rate minutes.
            yield env.timeout(random.expovariate(arrival_rate))
    
    env.process(passenger_generator(env))
    env.run(until=sim_time)
    
    avg_total = statistics.mean(waiting_times_total) if waiting_times_total else 0
    avg_id = statistics.mean(id_wait_times) if id_wait_times else 0
    avg_personal = statistics.mean(personal_wait_times) if personal_wait_times else 0
    return avg_total, avg_id, avg_personal, len(waiting_times_total)

# Define the Shiny UI using a simple fluid page layout.
app_ui = ui.page_fluid(
    ui.h2("Airport Security System Simulation"),
    ui.div(
         ui.input_numeric("num_id", "Number of ID/Boarding-Pass Checkers:", 4, min=1, max=20),
         ui.input_numeric("num_personal", "Number of Personal-Check Queues:", 4, min=1, max=20),
         ui.input_numeric("sim_time", "Simulation Time (minutes):", 100, min=10, max=1000),
         ui.input_numeric("arrival_rate", "Passenger Arrival Rate (per minute):", 5, min=1, max=200),
         ui.input_action_button("run_sim", "Run Simulation")
    ),
    ui.hr(),
    ui.div(
         ui.output_text("result")
    ),
    ui.hr(),
    ui.h3("Poisson Arrival Distribution"),
    ui.div(
         ui.output_plot("arrival_chart")
    ),
    ui.hr(),
    ui.h3("Average Wait Times at Each Stop"),
    ui.div(
         ui.output_plot("stops_chart")
    ),
    ui.hr(),
    ui.h3("Trend Analysis: Varying ID/Boarding-Pass Checkers"),
    ui.div(
         ui.output_plot("trend_id")
    ),
    ui.hr(),
    ui.h3("Trend Analysis: Varying Personal-Check Queues"),
    ui.div(
         ui.output_plot("trend_personal")
    )
)

def server(input, output, session):
    @reactive.Calc
    def simulation_result():
        # Only run the simulation after the button is pressed.
        if input.run_sim() == 0:
            return None
        num_id = input.num_id()
        num_personal = input.num_personal()
        sim_time = input.sim_time()
        arrival_rate = input.arrival_rate()
        return run_simulation(num_id, num_personal, sim_time, arrival_rate)
    
    @output
    @render.text
    def result():
        sim_result = simulation_result()
        if sim_result is None:
            return "Press 'Run Simulation' to start the simulation."
        avg_total, avg_id, avg_personal, n_passengers = sim_result
        return (f"Simulation complete over {n_passengers} passengers.\n"
                f"Average total time in system: {avg_total:.2f} minutes.\n"
                f"Average wait time for ID check: {avg_id:.2f} minutes.\n"
                f"Average wait time for Personal check: {avg_personal:.2f} minutes.\n"
                "Goal: Average wait time should be below 15 minutes.")
    
    @output
    @render.plot
    def arrival_chart():
        # Plot the Poisson PMF for the number of arrivals in 1 minute.
        lam = input.arrival_rate()
        x = list(range(0, 20))
        y = [poisson.pmf(k, lam) for k in x]
        fig, ax = plt.subplots()
        ax.bar(x, y, color='skyblue')
        ax.set_xlabel("Number of Arrivals in 1 minute")
        ax.set_ylabel("Probability")
        ax.set_title(f"Poisson Distribution (λ = {lam})")
        return fig
    
    @output
    @render.plot
    def stops_chart():
        # Bar chart showing the average wait times at each stop (from the last simulation).
        sim_result = simulation_result()
        if sim_result is None:
            return None
        _, avg_id, avg_personal, _ = sim_result
        labels = ['ID Check', 'Personal Check']
        times = [avg_id, avg_personal]
        fig, ax = plt.subplots()
        ax.bar(labels, times, color=['lightgreen', 'lightcoral'])
        ax.set_ylabel("Average Wait Time (minutes)")
        ax.set_title("Average Wait Time by Stop")
        ax.axhline(15, color='gray', linestyle='dotted', label='15 min threshold')
        ax.legend()
        return fig
    
    @output
    @render.plot
    def trend_id():
        # Trend chart: vary number of ID checkers (from 1 to 10) while holding the number of personal queues fixed.
        num_personal = input.num_personal()
        sim_time = input.sim_time()
        arrival_rate = input.arrival_rate()
        id_range = list(range(1, 11))
        avg_totals = []
        for num_id in id_range:
            avg_total, _, _, _ = run_simulation(num_id, num_personal, sim_time, arrival_rate)
            avg_totals.append(avg_total)
        fig, ax = plt.subplots()
        ax.plot(id_range, avg_totals, marker='o')
        ax.set_xlabel("Number of ID/Boarding-Pass Checkers")
        ax.set_ylabel("Average Total Time in System (minutes)")
        ax.set_title("Trend: Varying ID Checkers")
        ax.axhline(15, color='gray', linestyle='dotted', label='15 min threshold')
        ax.legend()
        return fig
    
    @output
    @render.plot
    def trend_personal():
        # Trend chart: vary number of personal-check queues (from 1 to 10) while holding the number of ID checkers fixed.
        num_id = input.num_id()
        sim_time = input.sim_time()
        arrival_rate = input.arrival_rate()
        personal_range = list(range(1, 11))
        avg_totals = []
        for num_personal in personal_range:
            avg_total, _, _, _ = run_simulation(num_id, num_personal, sim_time, arrival_rate)
            avg_totals.append(avg_total)
        fig, ax = plt.subplots()
        ax.plot(personal_range, avg_totals, marker='o')
        ax.set_xlabel("Number of Personal-Check Queues")
        ax.set_ylabel("Average Total Time in System (minutes)")
        ax.set_title("Trend: Varying Personal-Check Queues")
        ax.axhline(15, color='gray', linestyle='dotted', label='15 min threshold')
        ax.legend()
        return fig

app = App(app_ui, server)

if __name__ == "__main__":
    app.run()
