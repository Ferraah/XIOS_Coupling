import matplotlib.pyplot as plt
import numpy as np


def plot_xios_coupling(timesteps, coupl_freq, lag, restart_file=False, save_file=False, title="XIOS Coupling Diagram"):

    # Check for logically invalid parameters
    if lag and not restart_file:
        raise ValueError("Lag can only be used when restart_file is True.")
    if timesteps % coupl_freq != 0:
        raise ValueError("timesteps must be a multiple of coupl_freq.")
    # -------------------------

    # Set plot style and size
    figsize = (timesteps//1.5, 5)  # Width and height of the figure
    arrow_height = figsize[1] // 3  # Adjust arrow height based on figure height
    padding = 0.2 # Padding between the tick and the arrow
    _, ax = plt.subplots(figsize=figsize)  # Increased height by changing figsize
    plt.subplots_adjust(bottom=0.3)

    # Loading from file arrow
    if(restart_file):
        ax.arrow(-1, arrow_height, 1 + 0.1, figsize[1]-arrow_height, head_width=0.1, head_length=0.05, color='orange', label="xios_recv_field(field_restart)")
    # ------------------------

    # Upward arrows and Downward arrows
    for t in range(0, timesteps):
        # Downward red arrows slightly after the tick with shorter height
        ax.arrow(t + padding, figsize[1] - arrow_height, 0, arrow_height, head_width=0.1, head_length=0.05, 
            color=(
            'r' if not restart_file or t > 0 else 'gray'
            ),
            label=("xios_recv_field(field_recv)" if t == 1 
                else "")
            )

        # Upward blue arrows slightly before the tick with shorter height
        ax.arrow(t + 1 - padding, 0, 0, arrow_height, head_width=0.1, head_length=0.05, color='b', label="xios_send_field(field_send)" if t == 0 else "")
    
    # -------------------------
    

    # Display coupling (connect the arrows)
    for t in range(coupl_freq, timesteps, coupl_freq):
        # Draw a line connecting the arrowheads
        ax.plot([t - padding, t - 1 + padding + lag], [arrow_height, figsize[1] - arrow_height], color='gray', linestyle='--', alpha=0.7)
    # -------------------------

    # Save to file arrow
    if(save_file):
        ax.arrow(timesteps - padding, arrow_height, 1, figsize[1]-2*arrow_height, head_width=0.1, head_length=0.05, color='purple', label="Save to file by XIOS")
    # ------------------------- 

    # ---------------------------------------------------
    ax.set_xticks(range(0, timesteps+1))
    ax.set_yticks([])

    ax.tick_params(axis='x', which='both', top=True, bottom=True)
    ax.set_title(title, pad=20)  # Move title lower by increasing pad
    ax.set_xlabel("XIOS timestep", labelpad=20)
    for t in range(0, timesteps):
        ax.text(t + 0.5, -0.5, str(t+1), ha='center', va='center', color='black')
        ax.text(t + 0.5, figsize[1] + 0.5, str(t+1), ha='center', va='center', color='black')

    ax.set_xticklabels([])  # Hide major tick labels

    ax.grid(axis='x', linestyle='--', alpha=0.6)

    # ----------------------------------------------------

    # Add arrows legend
    plt.legend(loc='upper center', bbox_to_anchor=(0.5, -0.2),
          fancybox=True, shadow=True, ncol=2)


    plt.savefig("xios_coupling_plot.png")
    plt.close()

#plot_xios_coupling(timesteps=16, coupl_freq=4, lag=0, restart_file=False, save_file=False)
plot_xios_coupling(timesteps=16, coupl_freq=4, lag=1, restart_file=True, save_file=True)
