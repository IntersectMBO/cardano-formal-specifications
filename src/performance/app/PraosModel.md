# Introduction

Ouroboros Praos uses the distribution of 'stake' in the system (i.e. the
value of ADA controlled by each node) to randomly determine which node
(if any) is authorised to produce a new block in the chain during a
specific time interval (a 'slot'); the more stake a node controls, the
more likely it is to be authorised to produce a block. It is important
that the selected block-producing node has a copy of the most recently
produced block, so that the new block can correctly extend the previous
chain, otherwise there is a fork in the chain, meaning that at least one
of the blocks will be discarded, wasting work. Since the block producer
is selected at random, this means that the previous block needs to have
been copied to *all* block-producing nodes; we call this process 'block
diffusion'. For robustness, the consensus algorithm is designed to
withstand some imperfections in block diffusion, hence the effective
requirement is that blocks should be well-diffused "sufficiently often".
Put another way, the probability that a block fails to arrive in time
for the production of the next block must be suitably bounded. The
engineering challenge is to quantify this probability as a function of
the design and of the parameter choices of the implementation.

# Formulating the Problem

We assume that a collection of blockchain nodes is assembled into a
random graph (randomness is important in a blockchain setting for
mitigating certain adversarial behaviours). In each time slot, a
randomly-chosen node may generate a block, and we are interested in the
probability that the next randomly-chosen node has received that block
before it generates the next block. Since the granting of permission to
generate a block is random, the time between block generations is
exponentially distributed. In Cardano, slots are one second long and
blocks are produced every $20$ seconds on average.

::: statementbox
Problem Statement Starting from blockchain node $A$, what is the
probability distribution of the time taken for a block to reach a
different node $Z$, when $A$ and $Z$ are picked at random from the
graph?
:::

Since the graph is random with some limited node degree $N$, there is a
strong chance that $A$ is not directly connected to $Z$, and so the
block will have to pass through a sequence of intermediate nodes $B$,
$C$, ... The length of this sequence is a function of the size and node
degree of the graph [@small-worlds], and the (distribution of) time to
forward a block directly from one node to another is known (e.g., by
measurement).

# Modelling the Problem

Suppose for a moment that there are two hops to make from $A$ to $Z$:
first from $A$ to an intermediate node $B$; and, then, from $B$ to $Z$.
Using the $\Delta{}\textrm{Q}$SD notation from [@computers11030045], we
can write the corresponding outcome expression as
$o_{A \rightsquigarrow B} \ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}o_{B \rightsquigarrow Z}$,
where
"$\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}$\"
is the symbol we use for *sequential composition*: it means that the
outcome $o_{A}$ is followed by the outcome $o_{B}$. Likewise, the
outcome expression for three hops is
$o_{A \rightsquigarrow B} \ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}o_{B \rightsquigarrow C} \ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}o_{C \rightsquigarrow Z}$.
Generalising that to $n$ hops then is easy:
$o_{A \rightsquigarrow B_1} \ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}o_{B_1 \rightsquigarrow B_2} \ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}\dots \ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}o_{B_n \rightsquigarrow Z}$,
which we abbreviate as
$o_{A \rightsquigarrow B_1} \ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}(\ensuremath{\substack{\ensuremath{\mathbin{\bullet \hspace{-.2em} \rightarrow \hspace{-.25em} - \hspace{-.15em} \bullet}}\\ \ensuremath{\mathbin{\bullet \hspace{-.2em} \rightarrow \hspace{-.25em} - \hspace{-.15em} \bullet}}}_{1}^{n - 1}\ }o_{B_i \rightsquigarrow B_{i + 1}}) \ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}o_{B_n \rightsquigarrow Z}$.

Consider the two-hop scenario. Provided that we have
$\Delta{}\textrm{Q}$s for both $o_{A \rightsquigarrow B}$ and
$o_{B \rightsquigarrow Z}$, they can work out the
$\Delta{}\textrm{Q}$ of
$o_{A \rightsquigarrow B} \ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}o_{B \rightsquigarrow Z}$,
which is the convolution of the two constituent $\Delta{}\textrm{Q}$s:
$$\ensuremath{\Delta{}\textrm{Q}}{}(o_{A \rightsquigarrow B} \ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}o_{B \rightsquigarrow Z}) = \ensuremath{\Delta{}\textrm{Q}}{}(o_{A \rightsquigarrow B}) \ast \ensuremath{\Delta{}\textrm{Q}}{}(o_{B \rightsquigarrow Z})\text{.}$$
Using the *deltaq* package[^1], we can compute the convolution of two
$\Delta{}\textrm{Q}$s using the `.>>.` operator.

In practice, the time to transfer a block of data one hop depends on
four main factors:

1.  The size of the block;

2.  The speed of the network interface;

3.  The geographical distance of the hop (as measured by the time to
    deliver a single packet);

4.  Congestion along the network path.

When we consider blockchain nodes that are located in data centres
(which most block producers tend to be), the interface speed will
typically be 1Gb/s or more, which is not a significant limiting factor
in these circumstances. Likewise, congestion is generally minimal, and
so this can also be ignored in the first instance. This leaves: i) block
size, which we will take as a design parameter to be investigated later;
and ii) distance, which we will consider now. For simplicity, we will
consider three cases of geographical distance:

Short

:   : the two nodes are located in the same data centre;

Medium

:   : the two nodes are located in the same continent;

Long

:   : the two nodes are located in different continents.

Cardano relies on the standard TCP protocol for data transfers, because
it is widely supported on different platforms and can penetrate
firewalls. TCP transforms loss into additional delay, so the residual
loss is negligible. At this point, we could descend into a detailed
refinement of the TCP protocol, but equally we could simply take
measurements; the compositionality of $\Delta{}\textrm{Q}$SD means that
it makes no difference where the underlying values come from. Table
[1](#tab:one-hop-dq){reference-type="ref" reference="tab:one-hop-dq"}
shows measurements of the transit time of packets and the corresponding
transfer time of blocks of various sizes, using hosts running on AWS
data centre servers in Oregon, Virginia, London, Ireland and Sydney.
Since we know that congestion is minimal in this setting, the spread of
values will be negligible, and so in this case the CDFs for the
$\Delta{}\textrm{Q}$s will be step functions. The transfer time for each
block size is given both in seconds and in multiples of the basic
round-trip time (RTT) between the hosts in question. Since the TCP
protocol relies on the arrival of acknowledgements to permit the
transmission of more data, it is unsurprising to see a broadly linear
relationship, which could be confirmed by a more detailed refinement of
the details of the protocol.

::: {#tab:one-hop-dq}
  ------------ ------- ------- -------- ------- -------- ------- -------- -------- -------- -------- --------
                   RTT    64kB            256kB            512kB            1024kB            2048kB 
  Distance        secs    secs     RTTs    secs     RTTs    secs     RTTs     secs     RTTs     secs     RTTs
  **Short**      0.012   0.024   *1.95*   0.047   *3.81*   0.066   *5.41*    0.078   *6.36*    0.085   *6.98*
  **Medium**     0.069   0.143   *2.07*   0.271   *3.94*   0.332   *4.82*    0.404   *5.87*    0.469   *6.81*
  **Long**       0.268   0.531   *1.98*   1.067   *3.98*   1.598   *5.96*    1.598   *5.96*    1.867   *6.96*
  ------------ ------- ------- -------- ------- -------- ------- -------- -------- -------- -------- --------

  : Representative times in seconds and round-trip-times (RTTs) for
  one-way TCP transmission of varying block sizes for short, medium and
  long distances between blockchain nodes.
:::

We can encode the contents of Table
[1](#tab:one-hop-dq){reference-type="ref" reference="tab:one-hop-dq"}
using the as follows:\
(,)\
\[ \]\
\[ ,,,,\]\
\
\
\
\
\
\
\
\
\
\
\
\
\
\
\
\
\

If we assume that a direct TCP/IP connection between nodes has an equal
probability of being short, medium, or long, the probability
distribution of delay times for a single hop is\

We can then plot the CDF of the delay times for a single hop, shown in
Figure [1](#fig:one-hop-delays){reference-type="ref"
reference="fig:one-hop-delays"}:\
(()())

![One-Hop Delay Distributions per Block
Size](Inserts/oneHopDelays.pdf){#fig:one-hop-delays width="70%"}

The sequential compostion of a series of outcomes is given by the
$\Delta{}\textrm{Q}$ operation of *sequential composition*: \[ \]\
()() The distribution of delay times for a sequence of $n$ hops is then:

\
(()) Figure [2](#fig:multi-hop-64k){reference-type="ref"
reference="fig:multi-hop-64k"} shows the result of applying this to the
sequence of outcome expressions corresponding to one, two, ...five
sequential hops using the transfer delay distribution shown in
Figure [1](#fig:one-hop-delays){reference-type="ref"
reference="fig:one-hop-delays"}, for a 64kB block size, and
Figure [3](#fig:multi-hop-1024k){reference-type="ref"
reference="fig:multi-hop-1024k"} similarly for a 1024k block size (note
the difference in timescales between the two plots). In code this is: \[
\]\
\[ \]\
\
(()(()))\
\
(()(())) It can be seen that there is a $95\%$ probability of the block
arriving within $2$s for a 64k block, wheras for a $1024$kB block size
the $95^\mathit{th}$ percentile of transfer time is more than $5$s.

![Multi-Hop Delay Distributions for $64$k Block
Size](Inserts/multi-hop64k-plots.pdf){#fig:multi-hop-64k width="70%"}

![Multi-Hop Delay Distributions for $1024$k Block
Size](Inserts/multi-hop-1024k-plots.pdf){#fig:multi-hop-1024k
width="70%"}

If we know the distribution of expected path lengths, we can combine the
$\Delta{}\textrm{Q}$s for different hop counts using the
$\Delta{}\textrm{Q}$ operation of probabilistic choice. Table
[2](#tab:path-lengths){reference-type="ref"
reference="tab:path-lengths"} shows the distribution of paths lengths in
simulated random graphs having 2500 nodes and a variety of node degrees
[@path-lengths]. Using the path length distribution for nodes of degree
10, for example, then gives the transfers delay distribution shown in
Figure [4](#fig:multi-hop-all){reference-type="ref"
reference="fig:multi-hop-all"}., using the following code: \[
(,)\]\-- represent the column of the table\
\[ (,),(,),(,),(,),(,)\]\
\-- create a weighted sum of the hop distributions\
((,)(,))\
\
(()())

:::: center
::: {#tab:path-lengths}
  -------- ------------- ------- ------- -------
   Length    Node degree                 
                       5      10      15      20
     1              0.20    0.40    0.60    0.80
     2              1.00    3.91    8.58   14.72
     3              4.83   31.06   65.86   80.08
     4             20.18   61.85   24.95    4.40
     5             47.14    2.78    0.00 
     6             24.77    0.00         
     7              1.83                 
     8              0.05                 
  -------- ------------- ------- ------- -------

  : Percentage of Paths Having a Given Length in a Random Graph of
  $2500$ Nodes of Varying Degree
:::
::::

![Multi-Hop Delay Distributions per Block Size in a Graph of $2500$
Nodes of Degree
$10$](Inserts/blended-hop-blocksizes.pdf){#fig:multi-hop-all
width="70%"}

It can be seen that for a block to have a high probability of arriving
within the $2$s slot time, the block size must be not much more than
$64$kB.

## Verification Before Forwarding

So far, we have only considered the time taken to transfer a block from
one node to another. However, in the real system there are additional
steps:

1.  The block forging node, having been selected, must construct the
    block;

2.  Having constructed it, it must announce it to its neighbours;

3.  The recipient node must determine that the block is novel and
    request it;

4.  The block must be transferred;

5.  The recipient node must verify (adopt) the block before it can be
    announced.

Steps 3 - 5 are then repeated by each node in the path from the block
producer to the next block producer.

Many of these steps will depend on the size of the block and the
complexity of the scripts it contains. For example, the time taken to
verify a block will depend on the number and complexity of scripts in
the block. We will consider the block contents divided into two types:
*value* and *script*, with fixed sizes, ignoring mixed cases for
simplicity:\
(,)\
\[ \]\
\[ ,\]\
\
\
\
\

:::: center
::: {#tab:block-forging}
                                        Value    Script
  --------------------------------- --------- ---------
  Started forge loop iteration, s     0.00079   0.00071
  Acquired block context, s           0.02509   0.02339
  Acquired ledger state, s              6e-05     6e-05
  Acquired ledger view, s               2e-05     2e-05
  Leadership check duration, s        0.00043   0.00039
  Ledger ticking, s                   0.02785   0.02608
  Mempool snapshotting, s              0.0746   0.00225
  Leadership to forged, s             0.00087   0.00016
  Forged to announced, s              0.00073   0.00058
  Forged to sending, s                0.00759   0.00536
  Forged to self-adopted, s           0.08546   0.05759
  Slot start to announced, s          0.13048   0.05368

  : Timings for value- and script-heavy block forging in the 10.1.4 node
  release.
:::
::::

:::: center
::: {#tab:block-adoption}
                                   Value    Script
  ---------------------------- --------- ---------
  First peer notice, s            0.1326   0.05557
  First peer fetch, s            0.14433   0.06125
  Notice to fetch request, s     0.00146   0.00119
  Fetch duration, s              0.35826   0.12326
  Fetched to announced, s           -0.0     3e-05
  Fetched to sending, s          0.04552   0.04242
  Fetched to adopted, s          0.08461   0.05865

  : Timings for value- and script-heavy block adoption in the 10.1.4
  node release.
:::
::::

Using measurements taken from the benchmarking cluster[^2] for the
10.1.4 node release, as shown in tables
[3](#tab:block-forging){reference-type="ref"
reference="tab:block-forging"} and
[4](#tab:block-adoption){reference-type="ref"
reference="tab:block-adoption"}, we can give values for these functions:
\-- Leadership to forged\
\
\-- Forged to announced\
\
\-- Notice to fetch request\
\
\-- Fetched to adopted\
We can then combine these with the transfer delays to give the total
time for a block to be forged, transferred and verified from one node to
another:\
\
\
\
\
\
The time to transfer a block depends on its size, determined by the
contents of the block, and the length distribution of a hop, which for
the time being we will take to be the same as before:\
\
\
\
\
\
\
The total time for a block to be forged by the selected node and
diffused to the next selected node in a network with 2500 nodes of
degree 10 is then:\
\
\
\
((,)(,))\
\
(()())\
\
\
(()()) The CDF of the total time for a block of each type to be
transferred and verified from one node to another in a network with 2500
nodes of degree 10 is shown in Figure
[5](#fig:multi-hop-verified){reference-type="ref"
reference="fig:multi-hop-verified"}:

![Multi-Hop Delay Distributions per Block Type in a Graph of $2500$
Nodes of Degree
$10$](Inserts/verified-hop-blocksizes.pdf){#fig:multi-hop-verified
width="70%"}

## Header-Body Split {#Sect:Direction.2}

In Cardano Shelley, an individual block transmission involves a dialogue
between a sender node, $A$, and a recipient node, $Z$. We represent the
overall transmission as $o_{A \rightsquigarrow Z}$. This can be refined
into the following sequence:

1.  *P*ermission for *H*eader Transmission
    ($o_{Z \rightsquigarrow A}^{\mathit{ph}}$): Node $Z$ grants the
    permission to node $A$ to send it a header.

2.  *T*ransmission of the *H*eader
    ($o_{A \rightsquigarrow Z}^{\mathit{th}}$): Node $A$ sends a header
    to node $Z$.

3.  *P*ermission to for *B*ody Transmission
    ($o_{Z \rightsquigarrow A}^{\mathit{pb}}$): Node $Z$ analyses the
    header that was previously sent to it by $A$. Once the suitability
    of the block is determined via the header, node $Z$ grants
    permission to $A$ to send it the respective body of the previously
    sent header.

4.  *T*ransmission of the *B*ody
    ($o_{A \rightsquigarrow Z}^{\mathit{tb}}$): Finally, $A$ sends the
    block body to $Z$.

The motivation for the header/body split and the consequential dialogue
is optimisation of transmission costs. Headers are designed to be
affordably cheap to transmit. In addition, they carry enough information
about the body to enable the recipient to verify its suitability. The
body is only sent once the recipient has done this. This prevents the
unnecessary transmission of block bodies when they are not required.
Since bodies are typically several orders of magnitude larger than
headers, considerable network bandwidth can be saved in this way.
Moreover, the upstream node is not permitted to send another header
until given permission to do so by the downstream node, in order to
prevent a denial-of-service attack in which a node is bombarded with
fake headers, so this approach also reduces latency when bodies are
rejected. In practice, the first permission is sent when the connection
between peers is established, and the permission renewed immediately
after the header is received, so that the upstream peer does not have to
wait unnecessarily.

So we can refine $o_{A \rightsquigarrow Z}$ into
$o_{Z \rightsquigarrow A}^{\mathit{ph}} \ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}o_{A \rightsquigarrow Z}^{\mathit{th}} \ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}o_{Z \rightsquigarrow A}^{\mathit{pb}} \ensuremath{\mathbin{\bullet \hspace{-.6em} \rightarrow \hspace{-.85em} - \hspace{-.15em} \bullet}}o_{A \rightsquigarrow Z}^{\mathit{tb}}$.

[^1]: <https://hackage.haskell.org/package/deltaq>

[^2]: Thanks to Michael Karg for this data.
