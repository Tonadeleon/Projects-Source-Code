import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as anim
from matplotlib.animation import PillowWriter

# -----------------------------
# Define Custom Functions
# -----------------------------

def init_line(ax):
    """
    Initializes the trace line on the given axes.

    Parameters:
    - ax: Matplotlib Axes object.

    Returns:
    - trace: Line2D object representing the trace.
    """
    return ax.plot([], [], color='cyan', linewidth=1)[0]  # Thinner lines for aesthetics

def compute_F_n(n):
    """
    Computes F_n based on the given formula with t = n.

    Parameters:
    - n: Array of n values.

    Returns:
    - F_n: Array of complex F_n values.
    """
    # Avoid division by zero by ensuring n > 0
    n = np.where(n == 0, 1e-6, n)

    # Compute each component of the formula
    term1 = n**1.5
    term2 = (20 * np.cos(0.02 * n)) / (n + 100) * n  # Since t = n
    exponent = (10 * np.sin(0.4 * n) + 5 * np.cos(n * n)) * n + (0.03 * n * n + np.sin(n) / 10)

    # Combine terms to compute F_n
    F_n = (term1 + term2) * np.exp(1j * exponent)

    return F_n

def formula_animation(frame, trace, n, F_n):
    """
    Updates the trace data for the current frame based on the formula.

    Parameters:
    - frame: Current frame number.
    - trace: Line2D object to be updated.
    - n: Array of n values.
    - F_n: Array of complex F_n values.

    Returns:
    - trace: Updated Line2D object.
    """
    # Update the trace data up to the current frame
    trace.set_data(np.real(F_n[:frame]), np.imag(F_n[:frame]))

    return trace,

def init_figure_window():
    """
    Initializes the figure and axes with customized settings.

    Returns:
    - fig: Matplotlib Figure object.
    - ax: Matplotlib Axes object.
    """
    fig, ax = plt.subplots(figsize=(8, 8))  # Adjust size as needed

    # Set background colors to black
    fig.patch.set_facecolor('black')  # Figure background
    ax.set_facecolor('black')         # Axes background

    # Set limits based on expected size
    # You may need to adjust these based on the actual range of F_n
    ax.set_xlim(-1000, 1000)
    ax.set_ylim(-1000, 1000)

    # Remove axis ticks for a cleaner look
    ax.set_xticks([])
    ax.set_yticks([])

    return fig, ax

# -----------------------------
# Main Animation Setup
# -----------------------------

def main():
    # -----------------------------
    # Initialize Figure and Axes
    # -----------------------------
    fig, ax = init_figure_window()

    # -----------------------------
    # Initialize the Trace Line
    # -----------------------------
    trace = init_line(ax)

    # -----------------------------
    # Define Parameters for the Formula
    # -----------------------------
    # Define the range of n values
    n_min = 0       # Start from 0
    n_max = 300     # Adjust based on desired complexity
    num_n = 10000   # Number of n points for smoothness
    n = np.linspace(n_min, n_max, num_n)

    # -----------------------------
    # Compute F_n Values
    # -----------------------------
    print("Computing F_n values...")
    F_n = compute_F_n(n)
    print("Computation complete.")

    # -----------------------------
    # Create Animation Function Wrapper
    # -----------------------------
    def animate_wrapper(frame):
        """
        Wrapper function for the animation to pass the necessary parameters.

        Parameters:
        - frame: Current frame number.

        Returns:
        - trace: Updated Line2D object.
        """
        return formula_animation(frame, trace, n, F_n)

    # -----------------------------
    # Create Animation
    # -----------------------------
    print("Creating animation...")
    total_frames = num_n
    animation = anim.FuncAnimation(
        fig,
        animate_wrapper,
        frames=total_frames,
        interval=1,        # Interval in milliseconds
        blit=True          # Enable blitting for performance
    )
    print("Animation created.")

    # -----------------------------
    # Save Animation as GIF using PillowWriter
    # -----------------------------
    print("Saving animation as GIF...")
    writer = PillowWriter(fps=60)
    animation.save('custom_formula_animation.gif', writer=writer)
    print("Animation saved as 'custom_formula_animation.gif'.")

    # -----------------------------
    # Display the Animation
    # -----------------------------
    plt.show()

# -----------------------------
# Run the Animation
# -----------------------------

if __name__ == "__main__":
    main()
