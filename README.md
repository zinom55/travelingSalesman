# Traveling salesman problem

## Project 
The traveling salesman problem (TSP) is a long-standing and often discussed issue in computational physics and operations research: a traveling salesperson has to find the shortest closed tour between a ceratin set of cities, where each city is only visited once during the tour.

Naturally, this problem has applications in many every-day problems, e.g. in finding directions using a route guiding system. Optimizations can be archieved with respect to differen funcitons, e.g. the shortest path, the quickest path, and others.

The following development of the algorithm is mostly inspired by a paper "Optimization of the time-dependent traveling salesman problem with Monte Carlo method" by Johannes Bentner et al. (**paper.pdf**).

The data which is used, can be found in the folder **data/** and consits out of 127 beer garden, distributed around Augsburg, Germany.

--- 

**bier127.mp4** shows a video of the actual algorithm, while finding the optimized path with respect to the distance. The final result can be seen in **bier.pdf**. In In the following a rough explanation of the implementation is given. 

##Method
#Simulated Anealing
Simulated Annealing is a Monte-Carlo-method inspired by actual physics. It is used to find an approximate optimum of a function in solution space. The general idea is starting off by setting the system,which one wishes to observe, to a well chosen starting temperature $T_\text{start}$. The system should be in equilibrium state at the beginning of the observation. This can be assumed to be the case if the starting temperature is large compared to the maximal possible energy-fluctuations of the system. The measure one uses to define the optimum is the energy of the system. Therefore one wants to optimize\footnote{minimize or maximize} the function in accordance to the energy of the system, where the system mean energy $\bar{\mathcal{H}}$ at temperature $T$ can be calculated as: 
\begin{equation}
\bar{\mathcal{H}} = \int \text{d}x \; \mathcal{H}(x) \;e^{-\frac{\mathcal{H} (x)}{T}}
\end{equation}
where $\mathcal{H}(x)$ is the function that is being  optimized and $x$ the parameter, that $\mathcal{H}(x)$ is being optimized for.
The clue is to decrease the temperature $T$ towards a value close to 0. With the decreasing temperature the system cools down, meaning that the system energy decreases until it hits a local or, if one is lucky, the global minimum, which would also be the ground state of the system. 
#Algorithm
The computational algorithm is as follows.
One starts of by choosing a random system setting $x_i$, in case of a TSP one would start of by choosing a random permutation of the sequence in which the cities will be visited. The energy $\mathcal{H}(x_i)$ ( which symbolizes the distance in the case of a general TSP) is then calculated for this setting of the system. A new setting $x_{i+1}$ is drawn and the corresponding energy $\mathcal{H}(x_{i+1})$ calculated. Depending on the energy of $x_{i+1}$ the setting is accepted or not. If the energy $\mathcal{H}(x_{i+1})$ is smaller than the energy $\mathcal{H}(x_{i})$ the new setting $x_{i+1}$ always gets accepted. Else if the new energy $\mathcal{H}(x_{i+1})$ is larger than the energy of the previous setting $\mathcal{H}(x_{i})$ the new setting $x_{i+1}$ gets accepted with the probability
\begin{equation}\label{equ:accept}
	P_{\text{acc}} = \text{min}\left(exp\left(-\frac{\Delta \mathcal{H}}{T}\right),1\right),
\end{equation}
where $\Delta \mathcal{H} = \mathcal{H}(x_{i+1})- \mathcal{H}(x_{i})$. This allows the system to leave its current potentially local minimum to find a new one. If $x_{i+1}$ is not accepted, $x_{i+1}$ is discarded, while the old setting $x_{i}$ is kept. A new setting is then drawn and the process is repeated for a fix number of times $I$. After this $I$ times the temperature $T$ is decreased and the process repeated until convergence is reached. During the decrease of the temperature $T$, less new settings $x_{i+1}$ are accepted if their energy $\mathcal{H}(x_{i+1})$ is higher than the previous. Therefore it gets less likely to walk out of a local minimum for low temperatures. The system wanders only locally over the solution space, while for larger $T$ it still travels over greater areas.
There are several open question such as how to reduce the temperature and what effects does this scheme of reduction of, or how to chose the number of iterations $I$ per temperature step.

#Lowering the temperature
There are several ways of decreasing the temperature. This is crucial, since it is influencing how fast the algorithm is confined in a shrinking area in solution space. Two ways suggested in \cite{Urbach} are to lower the temperature as follows
\begin{equation}
    T_{j}=\frac{\Gamma}{\log(j)} \quad \text{or} \quad T_{j}=\alpha^j T_{\text{start}}, \quad (0 \leq \alpha \geq 1),
\end{equation}
where $\Gamma$ and $\alpha$ can be tuned for the best result.

In the following analysis we have chosen the second way ($T_{j}=\alpha^j T_{\text{start}}$) to lower the temperature.
