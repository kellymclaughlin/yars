Yars
============

http://github.com/mclaughlin77/yars

By Kelly McLaughlin

Hat Tip: Fuzed authors Dave Fayram, Tom Preston-Werner, and Abhay Kumar


Description
-----------

Yars provides a means to use the high performance Erlang webserver 
[Yaws] [] to serve rack applications in the same 
vein as Phusion Passenger. The main goals of this project are 
performance and simplicity. Yars strives to be simple 
to install, configure, and use so developers can spend more time
writing applications. 

Dependencies
-------
    
* [Erlang/OTP] []
* [Yaws] []
* [RVM] []
* Ruby 1.8.7 - rvm install 1.8.7
* [Bundler] []

Install
-------

Clone the yars repo.

    git clone http://github.com/mclaughlin77/yars.git

The master branch will always track the latest release.

Next, enter the yars directory and run make to build the 
erlang project files. 

Finally add any gems your rack-based project requires to 
the Gemfile and run:
    bundle install

This will install all the gems needed by yars and your 
project into the yars gemset.

That's it. Yars should be ready to configure and run. 

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
- *request_pool_size* - This is the number of ruby workers that yars will start and use to fulfill requests.
- *request_wait_threshold* - The amount of time in milliseconds that pending requests for a particular ruby instance will wait for it to finish its current processing task before being distributed to another member of the request pool. 
                           
Copyright
---------

Copyright (c) 2010 Kelly McLaughlin. See LICENSE for details.

[Erlang/OTP]: http://www.erlang.org "Erlang/OTP"
[Yaws]: http://yaws.hyber.org   "Yaws"
[RVM]: http://rvm.beginrescueend.com/   "RVM"
[Bundler]: http://gembundler.com    "Bundler"
