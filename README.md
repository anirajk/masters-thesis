

# MAKING LARGE TRANSFERS FAST FOR IN-MEMORY DATABASES IN MODERN NETWORKS
## School of Computing, University of Utah

Advisor: Ryan Stutsman

Defended: Thursday, February 23, 2017, 10 AM at MEB 3147, LCR

## Abstract:

Efficient movement of massive amounts of data over high-speed networks at high throughput is essential for a modern day In-memory storage system.

In response to the growing needs of throughput and latency demands at scale, a new class of database systems was developed in recent years. The development of these systems was guided by increased access to high throughput, low latency network fabrics and declining cost of Dynamic Random Access Memory (DRAM).

These systems were designed with On-Line Transactional Processing (OLTP) work- loads in mind, and, as a result, are optimized for fast dispatch and perform well under small request-response scenarios. However, massive server responses such as those for range queries and data migration for load balancing poses challenges for this design.

This thesis analyzes the effects of large transfers on scale-out systems through the lens of a modern Network Interface Card (NIC). The present-day NIC offers new and exciting opportunities and challenges for large transfers, but using them efficiently requires smart data layout and concurrency control.

We evaluated the impact of modern NICs in designing data layout by measuring transmit performance and full system impact by observing the effects of Direct Memory Access (DMA), Remote Direct Memory Access (RDMA) and caching improvements such as In- tel®Data Direct I/O (DDIO).

We discovered that use of techniques such as Zero Copy yield around 25% savings in CPU cycles and a 50% reduction in the memory pressure on the server by using a client-assisted design with records that are not updated in place.

We also set up experiments that underlined the bottlenecks in the current approach to data migration in RAMCloud and propose guidelines for a fast and efficient migration protocol for RAMCloud.



### List of publications relevant to Thesis:

1) A. Kesavan, R. Ricci, and R. Stutsman, To copy or not to copy: Making in-memory databases fast on modern NICs, in Proceedings of the Fourth International Workshop on In-Memory Data Management and Analytics (IMDM), Sept. 2016 


Thanks to John Moeller for the thesis format at the style files that helped me start with the writing tasks. Check back previous commits and README.md if you are looking for thesis templates for University of Utah (2016 Format)

<!-- Updated README to customise to my thesis, Aniraj Kesavan, 2017-2-23 -->
<!-- Converting to markdown, John Moeller, 2016-7-19 -->
<!-- Edit by Nelson H. F. Beebe <beebe@math.utah.edu> -->

