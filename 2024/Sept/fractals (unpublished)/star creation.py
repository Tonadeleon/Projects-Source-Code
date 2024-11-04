import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as anim
from matplotlib.animation import PillowWriter

# -----------------------------
# Define Custom Functions
# -----------------------------

# 1
def initLine(ax):
    """
    Initializes the trace line on the given axes.

    Parameters:
    - ax: Matplotlib Axes object.

    Returns:
    - trace: Line2D object representing the trace.
    """
    return ax.plot([], [], color='white', linewidth=0.5)[0]

# 2
def spiroanimation(frame, trace, z):
    """
    Updates the trace data for the current frame.

    Parameters:
    - frame: Current frame number.
    - trace: Line2D object to be updated.
    - z: Array of complex numbers representing the path.

    Returns:
    - trace: Updated Line2D object.
    """
    current_index = min(frame, len(z))
    
    trace.set_data(np.real(z[:current_index]), np.imag(z[:current_index]))
    
    return trace,

# 2
def initFigureWindow():
    """
    Initializes the figure and axes with customized settings.

    Returns:
    - fig: Matplotlib Figure object.
    - ax: Matplotlib Axes object.
    """
    fig, ax = plt.subplots(figsize=(10, 10)) 

    try:
        fig.canvas.manager.window.geometry('+1400+100')
    except AttributeError:
        pass
    
    fig.patch.set_facecolor('k') 
    ax.set_facecolor('k')
    
    ax.set_xlim(-10, 10)                
    ax.set_ylim(-10, 10)                
    
    ax.set_xticks([])                   
    ax.set_yticks([])                   
    
    return fig, ax

# 3
def exponential_ease(t, acceleration_factor):
    """
    Exponential easing function that accelerates the animation.

    Parameters:
    - t: Normalized time array (0 to 1).
    - acceleration_factor: Controls the rate of acceleration.

    Returns:
    - progress: Adjusted progress values based on the exponential easing.
    """
    t = np.clip(t, 1e-6, 1)
    
    progress = (np.exp(acceleration_factor * t) - 1) / (np.exp(acceleration_factor) - 1)
    
    return progress

# -----------------------------
# Main Animation Setup
# -----------------------------

def main():

    fig, ax = initFigureWindow()

    trace = initLine(ax)

    r1 = 4
    r2 = 4
    r3 = 1.3
    w1 = 44
    w2 = -17
    w3 = -54
    p1 = 0
    p2 = 0
    p3 = 0

    iterations = 4
    theta = np.linspace(0, 2 * np.pi * iterations, 50000)
    
    # -----------------------------
    # Main equation for my spirograph
    # -----------------------------
    z = (
        r1 * np.exp(1j * (w1 * theta + p1)) +
        r2 * np.exp(1j * (w2 * theta + p2)) +
        r3 * np.exp(1j * (w3 * theta + p3))
    )
    
    rotation_angle = np.pi / 2
    z = z * np.exp(1j * rotation_angle)
    
    # ease
    final_duration = 20 
    fps = 30
    acceleration_factor = 6
    total_frames = int(final_duration * fps)
    t = np.linspace(0, 1, total_frames)
    
    progress_values = exponential_ease(t, acceleration_factor)
    
    indices = (progress_values * len(theta)).astype(int)
    indices = np.clip(indices, 0, len(theta) - 1)
    
    # -----------------------------
    # Animation Function Wrapper
    # -----------------------------
    def animate_wrapper(frame):
        """
        Wrapper function for the animation to pass the necessary parameters.

        Parameters:
        - frame: Current frame number.

        Returns:
        - trace: Updated Line2D object.
        """
        return spiroanimation(indices[frame], trace, z)
    
    # -----------------------------
    # Create Animation
    # -----------------------------
    spiro_animation = anim.FuncAnimation(
        fig,
        animate_wrapper,
        frames=total_frames,
        interval=1000 / fps,
        blit=True
    )
    
    writer = PillowWriter(fps=fps)
    spiro_animation.save('star_spirograph_animation.gif', writer=writer)

    plt.show()

if __name__ == "__main__":
    main()