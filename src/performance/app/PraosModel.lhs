\documentclass[oneside,11pt]{article}
% NB This document contains Haskell code that generates the figures: see the README for instructions on how to run it.
\usepackage[a4paper, portrait, margin=2.5cm]{geometry}
%include polycode.fmt
\usepackage{array,multirow,hyperref,booktabs} 
\usepackage{graphicx} 
% Local definitions
\graphicspath{ {./Inserts/}}
\usepackage[most]{tcolorbox}
\makeatletter
\newcommand{\labitem}[2]{%
\def\@itemlabel{\textbf{#1.}}
\item
\def\@currentlabel{#1}\label{#2}}
\makeatother

\newcommand{\dq}{\ensuremath{\Delta{}\textrm{Q}}}
\newcommand{\DeltaQ}[1]{\ensuremath{\Delta{}\textrm{Q}_{\vert{}#1}}}
\newcommand{\dqG}{\DeltaQ{G}}
\newcommand{\dqS}{\DeltaQ{S}}
\newcommand{\dqV}{\DeltaQ{V}}
\newcommand{\dqGS}{\DeltaQ{G,S}}
\newcommand{\dqGSV}{\DeltaQ{G,S,V}}
\newcommand{\maxdq}{\ensuremath{\left\lceil\Delta{}\textrm{Q}\right\rceil}}
\newcommand{\dqsd}{\dq{}SD}

\newcommand{\BlackBox}{\ensuremath{\flat}}
\newcommand{\SeqDelta}{\ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}}
\newcommand{\ProbChoice}[2]{\ensuremath{\mathbin{\substack{#1 \\ {\displaystyle \leftrightharpoons} \\ #2}}}}
\newcommand{\DenSem}[1]{\ensuremath{[\hspace{-0.25em}[#1]\hspace{-0.25em}]}}
\newcommand{\Hazard}[2]{\ensuremath{\mathsf{hazard}_{#1}(#2)}}
\newcommand{\Slack}[2]{\ensuremath{\mathsf{slack}_{#1}(#2)}}
\newcommand{\AllOVs}{\ensuremath{\mathbb{O}_v}}
\newcommand{\Base}{\ensuremath{\overline{\mathbb{B}}}}
\newcommand{\NamedSem}[2]{\ensuremath{#1\DenSem{#2}}}
\newcommand{\DQAnalysis}[2]{\ensuremath{\NamedSem{\dq{}}{#1}}_{#2}}
\newcommand{\Resources}{\ensuremath{\mathbb{H}}}
\newcommand{\AmountOfWorkFor}[1]{\ensuremath{#1 \dststile{}{W}}}
\newcommand{\LoadAnalysis}[3]{\ensuremath{\AmountOfWorkFor{#1} \NamedSem{S}{#2}_{#3}}}
\newcommand{\SmallSeqDelta}{\ensuremath{\mathbin{\bullet \hspace{-.2em} \rightarrow \hspace{-.25em} - \hspace{-.15em} \bullet}}}
\newcommand{\MultiSeq}[2]{\ensuremath{\substack{\SmallSeqDelta \\ \SmallSeqDelta}_{#1}^{#2}\ }}
\newcommand{\MultiToFinish}[1]{\ensuremath{\mathopen{\parallel^{#1}}}}
\newcommand{\MultiFTF}{\MultiToFinish{\exists}}
\newcommand{\MultiATF}{\MultiToFinish{\forall}}
\newcommand{\Conv}{\mathop{\scalebox{1.5}{\raisebox{-0.2ex}{$\ast$}}}}

\definecolor{UnicornMilkCol}{rgb}{0.96,0.95,0.76}
\definecolor{SteelBlueCol}{rgb}{0.27,0.50,0.70}
\definecolor{SlateGrey}{rgb}{0.43,0.5,0.56}
\definecolor{DarkNavy}{rgb}{0.15, 0.25, 0.34}
\definecolor{purple}{rgb}{.5,.0,.5}
\newcommand{\Purple}[1]
{\textcolor{purple}{#1}}
\newcommand\rulefont[1]{\ensuremath{\Purple{\bf (\textsc{#1})}}}
\newcommand{\ActiveBox}[1]{\colorbox{UnicornMilkCol}{\ensuremath{#1}}}
\newtcolorbox{statementbox}[2][]{
  colback=black!5!white,
  colframe=black!75!black,
  colbacktitle=black!85!black,enhanced,
  attach boxed title to top center={yshift=-2mm},
  title={#2},#1
}
\title{Modelling Block Diffusion in Cardano using \dq{}}
\author{Peter Thompson\\Predictable Network Solutions Ltd.}
\date{May 2025}

\begin{document}
\maketitle
%if False
\begin{code}
{-# LANGUAGE FlexibleContexts #-}
module PraosModel 
    (   -- export the cdf plots
        oneHopCDF,
        multiHopCDF64k,
        multiHopCDF1024k,
        blendedHopCDFNode10,
        blendedHopCDFNode10',
        pipelindedMultiHopScript,
        pipelindedMultiHopValue,
        pipelindedMultiHopBounding,
        comparedCDFNode10
    )
where
import DeltaQ
import Graphics.Rendering.Chart (Layout)
\end{code}
%endif
\tableofcontents
\listoffigures
\listoftables
\paragraph{Note on the generation of this document}
The Haskell code in this document generates the figures that are included in the text.
The code is written in a literate style, with the code blocks interspersed with the text.
A README file is included in the repository that explains how to run the code to generate the figures,
and how to produce the final pdf.
Readers are invited to clone the repository and experiment with changing parameters to see how the figures change.
\section{Introduction}\footnote{The early sections of this document are largely reproduced from \cite{computers11030045}.}
Ouroboros Praos uses the distribution of `stake' in the system (i.e. the value of ADA 
controlled by each node) to randomly determine which node (if any) is authorised to 
produce a new block in the chain during a specific time interval (a `slot');
the more stake a node controls, the more likely it is to be authorised to produce a block
(see Appending \ref{sec:praos_leadership}).
It is important that the selected block-producing node has a copy of the 
most recently produced block, so that the new block can correctly extend the previous chain,
otherwise there is a fork in the chain, meaning that at least one of the blocks will be discarded, wasting work.
Since the block producer is selected at random, this means that the previous block needs to 
have been copied to \emph{all} block-producing nodes; we call this process `block diffusion'. 
For robustness, the consensus algorithm is designed to withstand some 
imperfections in block diffusion, hence the effective requirement is that blocks should
be well-diffused ``sufficiently often''.
Put another way, the probability that a block fails to arrive in time for
the production of the next block must be suitably bounded. 
The engineering challenge is to quantify this probability as a function of the design and of the
parameter choices of the implementation.

\section{Formulating the Problem}
We assume that a collection of blockchain nodes is assembled into a random graph (randomness is important in
a blockchain setting for mitigating certain adversarial behaviours).
In each time slot, a randomly-chosen node may generate a block, and we are interested in 
the probability that the next randomly-chosen node has received that block before it generates the 
next block. Since the granting of permission to generate a block is random, the time between
block generations is exponentially distributed.
In Cardano, slots are one second long and blocks are produced every $20$ seconds on average.

Given that the newly generated block takes some time to propagate through the network to the next 
block-producing node, a fork will be avoided if and only if there is no leader elected in the intervening period.
The probability of this is computed in Appendix \ref{sec:praos_leadership}.
If the probability that the diffusion takes at most $m$ slots is $P^D_m$, and the probability of $m$
successive slots with no leader is $P^{NL}_m$, then the probability $P^{NF}$ of avoiding a fork is:
\begin{equation}
P^{NF} = \sum_{m=0}^{\infty} P^D_m \times P^{NL}_m
\end{equation}
The main focus of this document is on the computation of $P^D_m$.

\vspace{24pt}

\begin{statementbox}[colback=white]{Diffusion problem Statement}
Starting from blockchain node $A$, what is the probability distribution of the time
taken for a block to reach a different node $Z$, when $A$ and $Z$ are picked at random from the graph?
\end{statementbox}
\vspace{24pt}

\noindent
Since the graph is random with some limited node degree $N$, there is a strong chance that
$A$ is not directly connected to $Z$, and so the block will have to pass through a sequence
of intermediate nodes $B$, $C$, \dots 
The length of this sequence is a function of the size and node degree of the graph \cite{small-worlds},
and the (distribution of) time to forward a block directly from one node to another is known (e.g., by measurement).

\section{Modelling the Problem}
Suppose for a moment that there are two hops to make from $A$ to $Z$:
first from $A$ to an intermediate node $B$; and, then, from $B$ to $Z$.
Using the \dqsd{} notation from \cite{computers11030045}, we can write 
the corresponding outcome expression as $o_{A \rightsquigarrow B} \SeqDelta o_{B \rightsquigarrow Z}$, 
where ``\SeqDelta" is the symbol we use for \textit{sequential composition}:
it means that the outcome $o_{A}$ is followed by the outcome $o_{B}$.
Likewise, the outcome expression for three hops is 
$o_{A \rightsquigarrow B} \SeqDelta o_{B \rightsquigarrow C} \SeqDelta o_{C \rightsquigarrow Z}$.
Generalising that to $n$ hops then is easy: 
$o_{A \rightsquigarrow B_1} \SeqDelta o_{B_1 \rightsquigarrow B_2} \SeqDelta \dots \SeqDelta o_{B_n \rightsquigarrow Z}$, 
which we abbreviate as 
$o_{A \rightsquigarrow B_1} \SeqDelta (\MultiSeq{1}{n - 1}o_{B_i \rightsquigarrow B_{i + 1}}) \SeqDelta o_{B_n \rightsquigarrow Z}$.

Consider the two-hop scenario.
Provided that we have \dq{}s for both $o_{A \rightsquigarrow B}$ and 
$o_{B \rightsquigarrow Z}$, they can work out the \dq{}\ of $o_{A \rightsquigarrow B} \SeqDelta o_{B \rightsquigarrow Z}$, 
which is the convolution of the two constituent \dq{}s:
$$
  \dq{}(o_{A \rightsquigarrow B} \SeqDelta o_{B \rightsquigarrow Z}) = \dq{}(o_{A \rightsquigarrow B}) \ast \dq{}(o_{B \rightsquigarrow Z})\text{.}
$$
Using the \textit{deltaq} package\footnote{\url{https://hackage.haskell.org/package/deltaq}}, 
we can compute the convolution of two \dq{}s using the {\tt .>>.} operator.

In practice, the time to transfer a block of data one hop depends on four main factors:
\begin{enumerate}
    \item The size of the block;
    \item The speed of the network interface;
    \item The geographical distance of the hop (as measured by the time to deliver a single packet);
    \item Congestion along the network path.
\end{enumerate}
When we consider blockchain nodes that are located in data centres (which most block producers tend to be), the
interface speed will typically be 1Gb/s or more, which is not a significant limiting factor in these circumstances.
Likewise, congestion is generally minimal, and so this can also be ignored in the first instance.
This leaves: i) block size, which we will take as a design parameter to be investigated later; 
and ii) distance, which we will consider now.
%
For simplicity, we will consider three cases of geographical distance:
\begin{description}
    \item [Short]: the two nodes are located in the same data centre;
    \item [Medium]: the two nodes are located in the same continent;
    \item [Long]: the two nodes are located in different continents.
\end{description}
Cardano relies on the standard TCP protocol for data transfers, because it is widely 
supported on different platforms and can penetrate firewalls. 
TCP transforms loss into additional delay, so the residual loss is negligible.
At this point, we could descend into a detailed refinement of the TCP protocol, but equally 
we could simply take measurements; the compositionality of \dqsd{} means that it makes no difference
where the underlying values come from. 
Table \ref{tab:one-hop-dq} shows measurements of the transit time of packets and 
the corresponding 
transfer time of blocks of various sizes, using hosts running on AWS data centre servers 
in Oregon, Virginia, London, Ireland and Sydney.
Since we know that congestion is minimal in this setting, the spread of values will be negligible, 
and so in this case the CDFs for the \dq{}s will be step functions.
The transfer time for each block size is given both in seconds and in multiples of the basic
round-trip time (RTT) between the hosts in question. 
Since the TCP protocol relies on the arrival of acknowledgements to permit the transmission of more
data, it is unsurprising to see a broadly linear relationship, which could be confirmed by a
more detailed refinement of the details of the protocol. 

\begin{table}[htb]
\begin{tabular}{lrrrrrrrrrrr}\toprule
& RTT & \multicolumn{2}{c}{64kB} & \multicolumn{2}{c}{256kB} &  \multicolumn{2}{c}{512kB} & \multicolumn{2}{c}{1024kB} & \multicolumn{2}{c}{2048kB} \\
Distance          & \multicolumn{1}{r}{secs} & \multicolumn{1}{l}{secs} & \multicolumn{1}{l}{RTTs} & \multicolumn{1}{l}{secs} & \multicolumn{1}{r}{RTTs} & \multicolumn{1}{l}{secs} & \multicolumn{1}{r}{RTTs} & \multicolumn{1}{l}{secs} & \multicolumn{1}{r}{RTTs} & \multicolumn{1}{l}{secs} & \multicolumn{1}{r}{RTTs} \\ \midrule
\textbf{Short}     & 0.012                        & 0.024                    & \textit{1.95}            & 0.047                     & \textit{3.81}            & 0.066                     & \textit{5.41}            & 0.078                      & \textit{6.36}            & 0.085                      & \textit{6.98}             \\
\textbf{Medium} & 0.069                        & 0.143                    & \textit{2.07}            & 0.271                     & \textit{3.94}            & 0.332                     & \textit{4.82}            & 0.404                      & \textit{5.87}            & 0.469                      & \textit{6.81}             \\
\textbf{Long}      & 0.268                        & 0.531                    & \textit{1.98}            & 1.067                     & \textit{3.98}            & 1.598                     & \textit{5.96}            & 1.598                      & \textit{5.96}            & 1.867                      & \textit{6.96}             \\ \bottomrule
\end{tabular}
\caption{Representative times in seconds and round-trip-times (RTTs) for one-way TCP transmission of varying block sizes for short, medium and long distances between blockchain nodes.}
\label{tab:one-hop-dq}
\end{table}

We can encode the contents of Table \ref{tab:one-hop-dq} using the as follows:
\begin{code}
data BlockSize = B64 | B256 | B512 | B1024 | B2048
    deriving (Show, Eq)
blockSizes :: [BlockSize]
blockSizes = [B64, B256, B512, B1024, B2048]
short :: BlockSize -> DQ
short B64     = wait 0.024
short B256    = wait 0.047
short B512    = wait 0.066
short B1024   = wait 0.078
short B2048   = wait 0.085
medium :: BlockSize -> DQ
medium B64    = wait 0.143
medium B256   = wait 0.271
medium B512   = wait 0.332
medium B1024  = wait 0.404
medium B2048  = wait 0.469
long :: BlockSize -> DQ
long B64      = wait 0.531
long B256     = wait 1.067
long B512     = wait 1.598
long B1024    = wait 1.598
long B2048    = wait 1.867
\end{code}

If we assume that a direct TCP/IP connection between nodes has an equal probability of being short,
medium, or long, the probability distribution of delay times for a single hop is
\begin{code}
hop :: BlockSize -> DQ
hop b = choices [(1, short b), (1, medium b), (1, long b)]
\end{code}

We can then plot the CDF of the delay times for a single hop, shown in Figure \ref{fig:one-hop-delays}:
\begin{code}
oneHopCDF :: Layout Double Double
oneHopCDF = plotCDFs "" (zip (map show blockSizes) (map hop blockSizes))
\end{code}
\begin{figure}[htb]
  \centering
        \includegraphics[width=0.7\textwidth]{oneHopDelays.pdf}
  \caption{One-Hop Delay Distributions per Block Size}
  \label{fig:one-hop-delays}
\end{figure}
The sequential compostion of a series of outcomes is given by the \dq{} operation of \textit{sequential composition}:
\begin{code}
doSequentially :: [DQ] -> DQ
doSequentially = foldr (.>>.) (wait 0)
\end{code}
The distribution of delay times for a sequence of $n$ hops is then:

\begin{code}
hops :: Int -> BlockSize -> DQ
hops n b = doSequentially (replicate n (hop b))
\end{code}
Figure \ref{fig:multi-hop-64k} shows the result of applying this to 
the sequence of outcome expressions corresponding to one, two, \dots five sequential hops
using the transfer delay distribution shown in Figure~\ref{fig:one-hop-delays}, for a
64kB block size, and Figure~\ref{fig:multi-hop-1024k} similarly for a 1024k block size
(note the difference in timescales between the two plots). 
In code this is:
\begin{code}
hopRange :: [Int]
hopRange = [1..5]
multiHopCDF64k :: Layout Double Double
multiHopCDF64k = plotCDFs "" (zip (map show hopRange) (map (`hops` B64) hopRange))
multiHopCDF1024k :: Layout Double Double
multiHopCDF1024k = plotCDFs "" (zip (map show hopRange) (map (`hops` B1024) hopRange))
\end{code}
It can be seen that there is a $95\%$ probability of the block arriving within $2$s for a 64k block,
wheras for a $1024$kB block size the $95^\mathit{th}$ percentile of transfer time is more than $5$s.

\begin{figure}[htbp]
  \centering
    \includegraphics[width=0.7\textwidth]{multi-hop64k-plots}
  \caption{Multi-Hop Delay Distributions for $64$k Block Size}
  \label{fig:multi-hop-64k}
\end{figure}

\begin{figure}[htbp]
  \centering
    \includegraphics[width=0.7\textwidth]{multi-hop-1024k-plots}
  \caption{Multi-Hop Delay Distributions for $1024$k Block Size}
  \label{fig:multi-hop-1024k}
\end{figure}

If we know the distribution of expected path lengths, we can combine the \dq{}s for
different hop counts using the \dq{} operation of probabilistic choice. 
Table \ref{tab:path-lengths} shows the distribution of
paths lengths in simulated random graphs having 2500 nodes and a variety of node degrees \cite{path-lengths}.
Using the path length distribution for nodes of degree 10, for example, then gives the transfers delay distribution 
shown in Figure \ref{fig:multi-hop-all}., using the following code:
\begin{code}
lengthProbsNode10 :: [(Int, Rational)] -- represent the column of the table
lengthProbsNode10 = [(1,0.40), (2,3.91), (3,31.06), (4,61.85), (5,2.78)]
blendedDelayNode10 :: BlockSize -> DQ -- create a weighted sum of the hop distributions
blendedDelayNode10 b = choices $ map (\(n,p) -> (p, hops n b)) lengthProbsNode10
blendedHopCDFNode10 :: Layout Double Double
blendedHopCDFNode10 = 
  plotCDFs "" (zip (map show blockSizes) (map blendedDelayNode10 blockSizes))
\end{code}
\begin{table}[hbt]
\begin{center}
\begin{tabular}{crrrr}
\toprule
Length & \multicolumn{4}{c}{Node degree}   \\
       & \multicolumn{1}{c}{5} & \multicolumn{1}{c}{10} & \multicolumn{1}{c}{15} & \multicolumn{1}{c}{20} \\ \midrule
1      & 0.20                  & 0.40                   & 0.60                   & 0.80                   \\
2      & 1.00                  & 3.91                   & 8.58                   & 14.72                  \\
3      & 4.83                  & 31.06                  & 65.86                  & 80.08                  \\
4      & 20.18                 & 61.85                  & 24.95                  & 4.40                   \\
5      & 47.14                 & 2.78                   & 0.00                   &                        \\
6      & 24.77                 & 0.00                   &                        &                        \\
7      & 1.83                  &                        &                        &                        \\
8      & 0.05                  &                        &                        &                        \\ 
\bottomrule
\end{tabular}
\caption{Percentage of Paths Having a Given Length in a Random Graph of $2500$ Nodes of Varying Degree}\label{tab:path-lengths}
\end{center}
\end{table}

\begin{figure}[htbp]
  \centering
    \includegraphics[width=0.7\textwidth]{blended-hop-blocksizes}
  \caption{Multi-Hop Delay Distributions per Block Size in a Graph of $2500$ Degree-$10$ Nodes}
  \label{fig:multi-hop-all}
\end{figure}
It can be seen that for a block to have a high probability of arriving within the $2$s slot time, 
the block size must be not much more than $64$kB.

We can confirm the effect of block size on the probability of a fork by combining the \dq{}
for the transfer delay with the probability of an $n$-slot gap in block production from Appendix 
\ref{sec:praos_leadership}. The probability of avoiding a fork is the sum of the probabilities of
avoiding a fork in each of the possible slots:
\begin{equation*}
P^{NF} = \sum_{n=0}^{\infty} P^D_n \times P^{NL}_n
\end{equation*}
where $P^D_n$ is the probability of a block being transferred in $n$ slots, 
and $P^{NL}_n$ is the probability of no leader being elected in $n$ slots.
The probability of a fork is one minus this.
\begin{code}
forkProbability  :: Rational           -- active slot fraction
                -> Rational          -- slot time
                -> DQ                -- transfer delay  
                -> Rational
forkProbability f slotTime d = 1 - probNoFork f slotTime d
  where
    probNoFork :: Rational -> Rational -> DQ -> Rational
    -- we accumulate probability over a finite number of slots, determined either by 
    -- diffusion certainty or by an arbitrary maximum number of slots
    probNoFork f' slotTime' d' = case deadline d' of
      -- if the diffusion can fail, we sum the probabilities up to an arbitrary cutoff
      Abandoned -> accumulateProbability f' slotTime' d' 100 -- ToDo: should depend on f
      -- if the diffusion probability reaches 1 by a time t, we can stop
      Occurs t  -> accumulateProbability f' slotTime' d' (ceiling (t/slotTime')) 
    accumulateProbability f' s d' n = 
    -- for each slot, we multiply the probability of no leader before that slot by the
    -- probability of successful diffusion within that time, and then sum over n slots
      sum (map (\i -> probNoLeader f' i * diffusionProbDensity d' s i) [1..n])
        where
          diffusionProbDensity d'' s' i' = 
            successWithin d'' (fromIntegral i' * s') - 
            successWithin d'' (fromIntegral (i' - 1) * s')
\end{code}
where $f$ is the active slot fraction, and $d$ is the \dq{} for the transfer delay.
Thus, for instance, the probability of a fork in a network of 2500 nodes of degree 10, with
a block size of 64kB, and active slot fraction of $0.01$ and a slot time of 2s, is 
%options ghci
\eval{fromRational (forkProbability 0.02 2 (blendedDelayNode10 B64)) :: Double}.

\subsection{Verification Before Forwarding}
So far, we have only considered the time taken to transfer a block from one node to another.
However, in the real system there are additional steps:
\begin{enumerate}
    \item The block forging node, having been selected, must construct the block;
    \item Having constructed it, it must announce it to its neighbours;
    \item The recipient node must determine that the block is novel and request it;
    \item The block must be transferred;
    \item The recipient node must verify (adopt) the block before it can be announced.
\end{enumerate}
Steps 3 - 5 are then repeated by each node in the path from the block producer to the next block producer.

Many of these steps will depend on the size of the block and the complexity of the scripts it contains.
For example, the time taken to verify a block will depend on the number and complexity of scripts in the block.
We will consider the block contents divided into two types: \textit{value} and \textit{script}, with fixed sizes,
we do not explicitly consider mixed cases for simplicity, but posit a synthetic bounding case that is
the worst of the two in all dimensions:
\begin{code}
data BlockContents = Value | Script | Bounding
  deriving (Show, Eq)
forgeBlock     :: BlockContents -> DQ
announceBlock  :: BlockContents -> DQ
requestBlock   :: BlockContents -> DQ
adoptBlock     :: BlockContents -> DQ
transferBlock  :: BlockContents -> DQ
\end{code}
\begin{table}[htb]
\begin{center}
\begin{tabular}{lrr}\toprule
 & Value & Script \\[0pt]
\midrule
Started forge loop iteration, s & 0.00079  & 0.00071 \\[0pt]
Acquired block context, s       & 0.02509  & 0.02339\\[0pt]
Acquired ledger state, s        & 6e-05    & 6e-05\\[0pt]
Acquired ledger view, s         & 2e-05    & 2e-05\\[0pt]
Leadership check duration, s    & 0.00043  & 0.00039\\[0pt]
Ledger ticking, s               & 0.02785  & 0.02608\\[0pt]
Mempool snapshotting, s         & 0.0746   & 0.00225\\[0pt]
Leadership to forged, s         & 0.00087  & 0.00016\\[0pt]
Forged to announced, s          & 0.00073  & 0.00058\\[0pt]
Forged to sending, s            & 0.00759  & 0.00536\\[0pt]
Forged to self-adopted, s       & 0.08546  & 0.05759\\[0pt]
Slot start to announced, s      & 0.13048  & 0.05368\\[0pt]
\bottomrule
\end{tabular}\caption{Timings for value- and script-heavy block forging in the 10.1.4 node release.}\label{tab:block-forging}
\end{center}
\end{table}

\begin{table}[htb]
\begin{center}
\begin{tabular}{lrr}\toprule
 & Value & Script \\[0pt]
\midrule
First peer notice, s        & 0.1326  & 0.05557\\[0pt]
First peer fetch, s         & 0.14433 & 0.06125 \\[0pt]
Notice to fetch request, s  & 0.00146 & 0.00119\\[0pt]
Fetch duration, s           & 0.35826 & 0.12326 \\[0pt]
Fetched to announced, s     & -0.0    & 3e-05\\[0pt]
Fetched to sending, s       & 0.04552 & 0.04242 \\[0pt]
Fetched to adopted, s       & 0.08461 & 0.05865\\[0pt]
\bottomrule
\end{tabular}\caption{Timings for value- and script-heavy block adoption in the 10.1.4 node release.}\label{tab:block-adoption}
\end{center}
\end{table}

Using measurements taken from the benchmarking cluster\footnote{Thanks to Michael Karg for this data.} 
for the 10.1.4 node release, as shown in tables
\ref{tab:block-forging} and \ref{tab:block-adoption}, we can give values for these functions:
\begin{code}
forgeBlock Value       = wait 0.00087 -- Leadership to forged
forgeBlock Script      = wait 0.00016
forgeBlock Bounding    = forgeBlock Script ./\. forgeBlock Value
announceBlock Value    = wait 0.00073 -- Forged to announced
announceBlock Script   = wait 0.00058
announceBlock Bounding = announceBlock Script ./\. announceBlock Value
requestBlock Value     = wait 0.00146 -- Notice to fetch request
requestBlock Script    = wait 0.00119
requestBlock Bounding  = requestBlock Script ./\. requestBlock Value
adoptBlock Value       = wait 0.08461 -- Fetched to adopted
adoptBlock Script      = wait 0.05865
adoptBlock Bounding    = adoptBlock Script .>>. adoptBlock Value -- need to validate both
\end{code}
We can then combine these with the transfer delays to give the total time for a block to be forged, 
transferred and verified from one node to another:
\begin{code}
firstHop :: BlockContents -> DQ
firstHop b = doSequentially [forgeBlock b, announceBlock b]
verifiedHop :: BlockContents -> DQ
verifiedHop b = 
  doSequentially [requestBlock b, transferBlock b, adoptBlock b, announceBlock b]
lastHop :: BlockContents -> DQ
lastHop b = doSequentially [requestBlock b, transferBlock b, adoptBlock b]
\end{code}
The time to transfer a block depends on its size, determined by the contents of the block, 
and the length distribution of a hop, which for the time being we will take to be the same as before:
\begin{code}
transferBlock b = choices [(1,short' b),(1,medium' b),(1,long' b)]
  where -- estimated values
    short' Value     = wait 0.3
    short' Script    = wait 0.01
    short' Bounding  = short' Script ./\. short' Value
    medium' Value    = wait 0.2
    medium' Script   = wait 0.05  
    medium' Bounding = medium' Script ./\. medium' Value
    long' Value      = wait 0.8
    long' Script     = wait 0.1
    long' Bounding   = long' Script ./\. long' Value
\end{code}
The total time for a block to be forged by the selected node and diffused to the next selected node 
in a network with 2500 nodes of degree 10 is then:
\begin{code}
totalTimeNode10 :: BlockContents -> DQ
totalTimeNode10 b = doSequentially [firstHop b, blendedDelayNode10' b, lastHop b]
  where
    blendedDelayNode10' :: BlockContents -> DQ
    blendedDelayNode10' b' = choices $ map (\(n,p) -> (p, hops' n b')) lengthProbsNode10
    hops' :: Int -> BlockContents -> DQ
    hops' n b'' = doSequentially (replicate (n - 1) (verifiedHop b''))
blendedHopCDFNode10' :: Layout Double Double
blendedHopCDFNode10' = 
  plotCDFs "" (zip (map show blockContents) (map totalTimeNode10 blockContents))
    where
      blockContents = [Value, Script, Bounding]
\end{code}
The CDF of the total time for a block of each type to be transferred and verified from one node to another in a 
network with 2500 nodes of degree 10 is shown in Figure \ref{fig:multi-hop-verified}:
\begin{figure}[htbp]
  \centering
    \includegraphics[width=0.7\textwidth]{verified-hop-blocksizes}
  \caption{Multi-Hop Delay Distributions per Block Type in a Graph of $2500$ Degree-$10$ Nodes}
  \label{fig:multi-hop-verified}
\end{figure}

\subsection{Header Pipelining} \label{Sect:Direction.2}

In Cardano Shelley, an individual block transmission involves a dialogue between a sender node, $A$, and a recipient node, $Z$.
%
We represent the overall transmission as $o_{A \rightsquigarrow Z}$. This can be refined into the following sequence:
%to demonstrate the scenario when the design engineer is about to study that  aspect of their system:
\begin{enumerate}
  \item \textit{P}ermission for \textit{H}eader Transmission ($o_{Z \rightsquigarrow A}^{\mathit{ph}}$):
    Node $Z$ grants the permission to node $A$ to send it a header.
  \item \textit{T}ransmission of the \textit{H}eader ($o_{A \rightsquigarrow Z}^{\mathit{th}}$):
    Node $A$ sends a header to node $Z$, which contains a summary of the block body: this is equivalent to the 'announce block' above.
  \item \textit{P}ermission to for \textit{B}ody Transmission ($o_{Z \rightsquigarrow A}^{\mathit{pb}}$):
    Node $Z$ analyses the header that was previously sent to it by $A$.
    Once the desirability of the block is determined via the header, node $Z$ sends permission to $A$ to 
    send it the respective body of the previously sent header: this is equivalent to the 'request block' above.
  \item \textit{T}ransmission of the \textit{B}ody ($o_{A \rightsquigarrow Z}^{\mathit{tb}}$):
    Finally, $A$ sends the block body to $Z$: this is equivalent to the 'transfer block' above.
\end{enumerate}

\noindent
The motivation for the header/body split and the consequential dialogue is optimisation of transmission costs
and prevention of denial-of-service attacks.
Headers are designed to be affordably cheap to transmit, in particular they fit inside a single IP packet,
and carry enough information about the block body to enable the recipient to verify its provenance and novelty.
The body is only sent once the recipient has requested it. 
Typically, the same header may be received from multiple sources, but the block body is requested from only one of them,
to avoid the unnecessary transmission of block bodies.
Since bodies are typically much larger than headers, this save considerable network bandwidth
and computation in block verification.

The upstream node is not permitted to send another header until given permission to
do so by the downstream node, in order to prevent a denial-of-service attack in which a
node is bombarded with fake headers, each of which takes computation to verify or discard.
In practice, the first header permission is sent when the connection between peers is established,
and the permission renewed immediately after the header is received, 
so that the upstream peer does not have to wait unnecessarily.

So we can refine $o_{A \rightsquigarrow Z}$ into
$o_{Z \rightsquigarrow A}^{\mathit{ph}} \SeqDelta o_{A \rightsquigarrow Z}^{\mathit{th}} \SeqDelta o_{Z \rightsquigarrow A}^{\mathit{pb}} \SeqDelta o_{A \rightsquigarrow Z}^{\mathit{tb}}$.

However, a minor compromise with regard to DoS resistance can reduce the latency of block propagation.
This is called `Header Pipelining' (or `Diffusion Pipelining'), in which a new header is forwarded to the next node before 
the body has been received, and the body is forwarded before it has been fully verified.
To contain the risk of DoS attacks, the recipient node will not request another header from the sending node
until the corresponding body has been received and verified, and every forwarding node must check:
\begin{enumerate}
  \item That the header is correct before forwarding: i.e. the block correctly references its predecessor, and has been generated according 
  to the Praos leadership schedule (verifiable-random-function (VRF) and block-signature validation);
  \item That the block is complete before forwarding, i.e. the received (but not yet validated) body is indeed referenced by the header's body hash.
\end{enumerate}
These ensure that an adversary can only inject malicious data to the extent that it controls stake.

The sequence of steps then becomes (where node $A$ is the block producer, node $B$ is a directly connected node, 
and node $Z$ is the next block producer):
\begin{enumerate}
    \item Node $A$, having been selected, constructs the block;
    \item It announces the new block to its neighbours (nodes $B$) by sending the header;
    \item Node $B$ validates the header and determines that it is novel (i.e. not already received from elsewhere);
    \item Node $B$ then concurrently:
    \begin{itemize}
        \item Requests the block body;
        \item Announces the header to its neighbours;
    \end{itemize}
    \item The block is transferred from $A$ to $B$;
    \item Node $B$ must check the block is complete;
    \item Node $B$ then concurrently:
    \begin{itemize}
        \item Verifies the block body and adopts it;
        \item Transfers the block to any neighbours that have requested it;
    \end{itemize}
    \item Node $Z$ must adopt the block before constructing the next block.
\end{enumerate}
The granting of permission to send the \textit{next} header is not on the critical path for delivering the block 
from $A$ to $Z$, so we will omit it for simplicity.
Verifying the header and checking the block is complete are (currently) relatively trivial operations, 
whose timing is not directly measured, so we assign them a nominal millisecond.
\begin{code}
validateHeader :: BlockContents -> DQ
validateHeader Value      = wait 0.0001 -- Assumed de minimus time
validateHeader Script     = wait 0.0001 -- Assumed de minimus time
validateHeader Bounding   = validateHeader Script ./\. validateHeader Value
checkBlock :: BlockContents -> DQ
checkBlock Value      = wait 0.0001 -- Assumed de minimus time
checkBlock Script     = wait 0.0001 -- Assumed de minimus time
checkBlock Bounding   = checkBlock Script ./\. checkBlock Value
\end{code}
The complication here is a \textit{last-to-finish} synchronisation between receiving and checking 
the block body from the upstream node and receiving the request for it from the downstream node; 
both are required before the block can be forwarded to the downstream node. 

Consider first of all the trivial case where the forging node $A$ and the next block producer $Z$ are directly connected:
\begin{code}
oneHopTransfer :: BlockContents -> DQ
oneHopTransfer b = doSequentially [forgeBlock b, announceBlock b,       -- done by node A
                                   validateHeader b, requestBlock b,    -- done by node Z
                                   transferBlock b,                     -- done by node A
                                   checkBlock b, adoptBlock b]          -- done by node Z
\end{code}
Now consider the case with an intermediate node $B$: node $B$ must check the block and node $Z$ must request the block,
having received and validated the header:
\begin{code}
twoHopTransfer :: BlockContents -> DQ
twoHopTransfer b = doSequentially [forgeBlock b, announceBlock b,                             -- done by node A
                                   validateHeader b, announceBlock b, requestBlock b,         -- done by node B
                                   transferBlock b,                                           -- done by node A
                                   checkBlock b  ./\.                                         -- done by node B
                                   (validateHeader b .>>. requestBlock b),                    -- done by node Z
                                   transferBlock b,                                           -- done by node B
                                   checkBlock b, adoptBlock b]                                -- done by node Z
\end{code}
We can generalise this to $n$ hops\footnote{This version due to Vashti Galpin} 
using recursion for both the header propagation and the block body transfers:
\begin{code}
passHeader :: Int -> BlockContents -> DQ -- pass the header along a path of length n
passHeader n b
| n <= 0 = wait 0
  | n == 1 = validateHeader b
  | otherwise = doSequentially [validateHeader b, announceBlock b, passHeader (n - 1) b]
  | n == 1 = validateHeader b
  | otherwise = doSequentially [validateHeader b, announceBlock b, passHeader (n - 1) b]

getBlock :: Int -> BlockContents -> DQ  -- pass the block along a path of length n
-- we must first receive the header before we can request the block from nodes that 
-- have already received the header and may have the block body
getBlock n b
  | n <= 0 = wait 0
  | otherwise = doSequentially [forgeBlock b, announceBlock b, passHeader n b, 
                                requestBlock b, goGetBlock n b]
  where
    goGetBlock :: Int -> BlockContents -> DQ 
    goGetBlock n b
      | n <= 0 = wait 0
      | n == 1 = doSequentially [transferBlock b, checkBlock b, adoptBlock b]
      | otherwise = 
        transferBlock b .>>. ((checkBlock b ./\. requestBlock b) .>>. goGetBlock (n - 1) b)
\end{code}
With these functions we can generate the CDFs for the total time for a block of each type to be 
transferred and verified from one node to another via a number of hops:
\begin{code}
pipelindedMultiHopScript :: Layout Double Double
pipelindedMultiHopScript = 
  plotCDFs "" ((zip (map show hopRange) (map (`getBlock` Script) hopRange)) ++
              [("One hop", oneHopTransfer Script), ("Two hops", twoHopTransfer Script)])
pipelindedMultiHopValue :: Layout Double Double
pipelindedMultiHopValue = 
  plotCDFs "" ((zip (map show hopRange) (map (`getBlock` Value) hopRange)) ++
              [("One hop", oneHopTransfer Value), ("Two hops", twoHopTransfer Value)])

pipelindedMultiHopBounding :: Layout Double Double
pipelindedMultiHopBounding = 
  plotCDFs "" ((zip (map show hopRange) (map (`getBlock` Bounding) hopRange)) ++
              [("One hop", oneHopTransfer Bounding), ("Two hops", twoHopTransfer Bounding)])
\end{code}
We can use a weighted choice of the number of hops, as before, 
to model the effect of the path length distribution:
\begin{code}
pipelinedTimeNode10 :: BlockContents -> DQ
pipelinedTimeNode10 b = choices $ map (\(n,p) -> (p, getBlock n b)) lengthProbsNode10
\end{code}
The CDF of the total time for a block of each type to be transferred and verified from one node to another  
over a series of hops using pipelining is shown in Figure \ref{fig:multi-hop-pipelined-script} for script-heavy blocks,
Figure \ref{fig:multi-hop-pipelined-value} for value-heavy blocks, and Figure \ref{fig:multi-hop-pipelined-bounding}
for the synthetic bounding case. Note that the lines for the individual one and two hop cases overlay those 
generated by the recursive general case.
\begin{figure}[htbp]
  \centering
    \includegraphics[width=0.7\textwidth]{pipelined-hop-script}
  \caption{Pipelined Delay Distributions for Script Block Type per Hop}
  \label{fig:multi-hop-pipelined-script}
\end{figure}
\begin{figure}[htbp]
  \centering
    \includegraphics[width=0.7\textwidth]{pipelined-hop-value}
  \caption{Pipelined Delay Distributions for Value Block Type per Hop}
  \label{fig:multi-hop-pipelined-value}
\end{figure}
\begin{figure}[htbp]
  \centering
    \includegraphics[width=0.7\textwidth]{pipelined-hop-bounding}
  \caption{Pipelined Delay Distributions for Bounding Block Type per Hop}
  \label{fig:multi-hop-pipelined-bounding}
\end{figure}
The pipelined case is compared with the non-pipelined case in Figure \ref{fig:multi-hop-compared}. 
It can be seen that there is a reduction in the time taken for a block to be transferred and verified
when pipelining is used, particularly for script-heavy blocks.
\begin{code}
comparedCDFNode10 :: Layout Double Double
comparedCDFNode10 = 
  plotCDFs "" (zip (map show blockContents) (map totalTimeNode10 blockContents) ++
               zip pipelinedLabels (map pipelinedTimeNode10 blockContents))
    where
      pipelinedLabels = map ((++ " (pipelined)") . show) blockContents
      blockContents = [Value, Script, Bounding]
\end{code}
\begin{figure}[htbp]
  \centering
    \includegraphics[width=0.7\textwidth]{compared-hop-blocktypes}
  \caption{Pipelined and un-piplined Delay Distributions per Block Type Compared}
  \label{fig:multi-hop-compared}
\end{figure}
\newpage
\appendix
\section{Praos Leader Selection}\label{sec:praos_leadership}
Ouroboros Praos has significant operational differences from typical consensus algorithms. 
Instead of a slot leader schedule being pre-computed, 
each stakeholder separately computes its own schedule, based on its own private key. 
Since the overall schedule is the result of independent presudo-random computations,
it is effectively a Poisson process.
This creates the potential for both leadership clashes (where two or more stakeholders 
are scheduled to produce a block in the same slot, referred to as `slot battles') 
and empty slots (where no stakeholder is scheduled to produce a block). 
In order to compensate for the empty slots, the slot time is kept short, so that the average rate 
of production of blocks is acceptable. The protocol is provably robust against 
message delays up to a parameter $\Delta$ (measured in slot-times), and its security 
degrades gracefully as the delays increase.

This Poisson process has implications for the performance of the overall Cardano system:
The non-uniform rate of production of blocks introduces a variable load on the block diffusion function;
The short slot time increases the probability that a block will not be fully diffused before the end 
of the slot (depending on the size of the block), and hence may not be available to a leader in the immediately 
following slot, causing a fork (referred to as a `height battle').
Conversely, long sequences of empty slots (which must occur from time to time) 
allow all previous blocks to be diffused to every node, 
ensuring a consistent view of the chain to be established.

This introduces a set of trade-offs, determined by Praos parameters:
\begin{itemize}
\item Slot time;    
\item Slot frequency;
\item Number of active nodes;
\item Block size;
\item Slot occupancy probability;
\item Maximum diffusion delay $\Delta$.
\end{itemize}

Which collectively affect the outcomes of the algorithm:
\begin{itemize}
\item Effective transaction rate;
\item Wait time for inclusion in the chain;
\item Probability of being included in a `losing' fork (requiring transaction resubmission);
\item Rate of growth of longest chain.
\end{itemize}
The parameters are summarised in Table \ref{tab:parameters}.
\begin{table}[htb]
\centering
\begin{tabular}{>{\hspace{0pt}}m{0.16\linewidth}>{\hspace{0pt}}m{0.477\linewidth}>{\hspace{0pt}}m{0.246\linewidth}>{\hspace{0pt}}m{0.025\linewidth}>{\hspace{0pt}}m{0.025\linewidth}} 
\toprule
\textbf{Parameter}                            & \textbf{Description}                                          & \textbf{Notes}                       &                                             &                                              \\
$N$                                           & Number of active nodes                                        & Expected to be \textasciitilde{}2000 & \multirow{3}{0.025\linewidth}{\hspace{0pt}} & \multirow{5}{0.025\linewidth}{\hspace{0pt}}  \\
$T_s$                                         & Duration of a slot                                            & Expected to be \textasciitilde{}1s  &                                             &                                              \\
$f$                                           & Active slot fraction                                          & $0 < f \leq 1$ (set at 1/20)         &                                             &                                              \\
$\Delta$                                      & Maximum number of slots before a diffused message is received & $\Delta \geq 1$                      & \multirow{2}{0.025\linewidth}{\hspace{0pt}} &                                              \\
\bottomrule
\end{tabular}\caption{Parameters for the Praos protocol.}
\label{tab:parameters}
\end{table}
\subsection{Distribution of leadership}
From \cite{10.1007/978-3-319-78375-8_3}, the probability of stakeholder $U_i$ with relative stake $\alpha_i$ 
being leader in any slot is:
\begin{equation}
p_i = \Phi_f(\alpha_i) = 1 - (1-f)^{\alpha_i}
\end{equation}
If each of the $N$ active nodes has an equal amount of stake, $\alpha_i=\frac{1}{N}$, 
and hence equal probability of being a leader in any particular slot, then we would have:
\begin{equation}
\forall_i\ {p_i} =  1 - (1-f)^{\frac{1}{N}}
\end{equation}
In general, the probability that stakeholder $U_i$ is \textit{not} the leader is $1-p_i = (1-f)^{\alpha_i}$, 
and so the probability that \textit{no} stakeholder is the leader (i.e. we have an empty slot) is
given by multiplying the probabilities (since each node decides independently whether it is the leader):
\begin{equation}
P_\text{no leader} = \prod_{i=1}^{N}(1-f)^{\alpha_i} = (1-f)^{\sum_{i=0}^N\alpha_i} = 1 - f
\end{equation}

(Hence the definition of $f$ as the active slot fraction). 
Note that this is independent of the actual distribution of stake.

Consequently, the probability of a run of $m$ successive empty slots (since these are independent trials) is:
\begin{equation*}
P^{NL}_m =P_\text{m empty slots} = P_\text{no leader}^m = (1-f)^m
\end{equation*}
We can render this in Haskell as:
\begin{code}
probNoLeader :: Rational -- active slot fraction
             -> Int      -- number of slots
             -> Rational -- probability of no leader
probNoLeader f m = (1 - f) ^ m
\end{code}

\bibliographystyle{plain}
\bibliography{Inserts/DeltaQBibliography,Inserts/AdditionalEntries}
\end{document}
