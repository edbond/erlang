#!/usr/bin/ruby

require 'rubygems'
require 'rr'
require 'net/http'

proxy_addr = 'localhost'
proxy_port = 3456

res=Net::HTTP::Proxy(proxy_addr, proxy_port).start('www.google.com') {|http|
  http.get('/')
}

puts res.inspect
