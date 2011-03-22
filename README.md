# Yesod MPC

### Description

Control a running MPD through a yesod subsite.

I chose to name this MPC and not MPD to prevent clashes with functions 
and types exposed by libmpd (which I'm using), and also it makes sense 
since this is a controller and not a daemon anyway.

### Usage

See ./Test.hs or read the 
[haddocks](http://pbrisbin.com/haskell/docs/html/yesod-mpc/)

### Try it

Assuming you've got the required dependencies you can run the Test app 
directly. If you don't, but are willing to install them, just `cabal 
install` from within the yesod-mpc directory.

    git clone git://github.com/pbrisbin/yesod-mpc.git
    cd yesod-mpc
    runhaskell Test.hs
    $BROWSER http://localhost:3000

Here's what it looks like on my server:

![Yesod MPC Shot](http://pbrisbin.com/static/fileshare/yesod_helpers_mpc.png)
