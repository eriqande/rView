rView
================

This is a package with a single function that hijacks the server used by
the rmote package to send data objects over the network to be viewed
using the RStudio viewer in a local session.

Here is what a session looks like on the Sedna compute cluster. Some of
this is a total workaround right now because the compute nodes can’t to
port forwarding, but the head node can.

1.  From RStudio terminal, login to sedna

2.  Get a compute node (`srun --pty /bin/bash`)

3.  On that compute node, start R and then `library(rmote)`

4.  Start the rmote server, but make sure that it is writing to a
    directory in my home
    directory:
    
    ``` sh
    rmote::start_rmote(server_dir = "/home/eanderson/rmote_server", port = 4321)
    ```

5.  In a separate terminal, login to the head node with:
    
    ``` sh
    ssh -L 4321:localhost:4321 eanderson@sedna.nwfsc2.noaa.gov
    ```
    
    And then start an R session there and give it this:
    
    ``` r
    library(rmote)
    rmote::start_rmote(server_dir = "/home/eanderson/rmote_server", port = 4321)
    ```
    
    I know we should run compute jobs on the head node, but this is
    merely serving up the directory on a forwarded port. Not compute
    intensive. Note that when we write things from the compute node to
    `/home/eanderson/rmote_server` they get served up by the head node.
    That seems OK. (and that is the workaround part for the fact that
    the compute nodes can’t forward ports.)

Then, when that is all set up, we can send objects to our local RStudio
like this:

  - First way, by object name:

<!-- end list -->

``` r
# on remote R session
rView(mtcars)

# then, on local session
rView(mtcars)
```

  - If the object to be view is piped in, then do like this:

<!-- end list -->

``` r
# on remote R session
library(tidyverse)
diamonds %>%
  filter(cut == "Ideal") %>%
  rView()
  
# then, on local session get it by:
rView(.)

# or by piping anything into rView()...
```
