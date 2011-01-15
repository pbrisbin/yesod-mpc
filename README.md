# Yesod MPC

### Description

Control a running MPD through a yesod subsite.

I chose to name this MPC and not MPD to prevent clashes with functions 
and types exposed by libmpd (which I'm using), and also it makes sense 
since this is a controller and not a daemon anyway.

### Usage

See ./Test.hs. Haddocks are coming.

### Try it

    git clone git://github.com/pbrisbin/yesod-mpc.git
    cd yesod-mpc
    runhaskell Test.hs
    $BROWSER http://localhost:3000/mpc

Here's what it looks like on my server:

![Yesod MPC Shot](http://pbrisbin.com/static/fileshare/yesod_helpers_mpc.png)
