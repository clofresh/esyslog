esyslog - an Erlang syslog server
=================================

esyslog aims to be an extensible and robust drop-in replacement for the standard syslogd daemon. 

Syslog is a network protocol for sending log messages. The current standard is [RFC 3164](http://tools.ietf.org/html/rfc3164). There are many implementations of the syslog protocol like syslogd, rsyslog and syslog-ng, but none of them are very friendly to extend unless you want to write C code. esyslog hopes to match previous implementations in features while taking advantage of Erlang's robustness and hot-code loading. esyslog will (eventually) be extensible via Erlang or a different high level dynamic language. 


Dependencies
------------
* [Rake](http://github.com/jimweirich/rake)
* [Erlang R13B03](http://www.erlang.org/download.html)

Running
-------
Assuming you have all the dependencies, you can run:

    rake start
  
Which will start a local esyslog server listening at port 7777, using the configuration at ./etc/syslog.conf

Roadmap
-------
* Improve syslog config parser to support all syslog.conf syntax
* Implement remote logging
* Create plugin framework


