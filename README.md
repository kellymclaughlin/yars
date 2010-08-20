Yeref
============

http://github.com/mclaughlin77/yeref

By Kelly McLaughlin

Hat Tip: Fuzed authors Dave Fayram, Tom Preston-Werner, and Abhay Kumar


Description
-----------

Yeref provides a means to use the high performance Erlang webserver 
[Yaws](http://yaws.hyber.org) to serve rack applications in the same 
vein as Phusion Passenger. The main goals of this project are 
performance and simplicity. Yeref strives to be simple 
to install, configure, and use so developers can spend more time
writing applications. 

Dependencies
-------
    
    [Erlang/OTP](http://www.erlang.org)
    [Yaws](http://yaws.hyber.org)
    [RVM](http://rvm.beginrescueend.com/)
    Ruby 1.8.7 - rvm install 1.8.7
    [Bundler](http://gembundler.com)

Install
-------

Clone the yeref repo.

    git clone http://github.com/mclaughlin77/yeref.git

The master branch will always track the latest release.

Next, enter the yeref directory and run make to build the 
erlang project files. 

Finally add any gems your rack-based project requires to 
the Gemfile and run:
    bundle install

This will install all the gems needed by yeref and your 
project into the yeref gemset.

That's it. Yeref should be ready to configure and run. 

Configuration
---------

Copy the conf/sample.conf file to use a template. Many
of the configuration directives are self-explanatory so
I'll only cover a few here.
    
- Create a server block for each application you 
  want to server. Replace localhost with the 
  domain name for the application.
- *docroot* - Change docroot to point to the public directory of
  your project.
- *request_pool_size* - This is the number of ruby workers that yeref will start and use to fulfill requests.
- *backup_request_pool_size* - This is the number of ruby workers that yeref will start and use to continue to serve requests promptly when there are slow requests pending on the ruby workers from the normal pool.  It is recommended that this number be at least half of the size of the request_pool_size parameter, but the best value will be dependent on the characteristics of your application and traffic flow.
- *request_wait_threshold* - The amount of time in milliseconds that pending requests for a particular ruby instance will wait for it to finish its current processing task before being distributed to the backup request pool. 
                           
Copyright
---------

Copyright (c) 2010 Kelly McLaughlin. See LICENSE for details.
