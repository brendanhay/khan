#!/usr/bin/env ruby

require 'webrick'

bind = '169.254.169.254'
dir  = File.expand_path(File.dirname(__FILE__))

system("ifconfig lo0 alias #{bind}")

server = WEBrick::HTTPServer.new({
  :BindAddress    => bind,
  :Port           => 80,
  :DocumentRoot   => dir,
  :RequestTimeout => 1
})

puts "Serving EC2 metadata from #{dir}"

trap('INT') do
  server.shutdown
  system("ifconfig lo0 -alias #{bind}")
end

server.start
