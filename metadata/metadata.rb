require 'rubygems'
require 'bundler/setup'
require 'eventmachine'
require 'rack'
require 'rubydns'
require 'thin'

PORT     = 80
IN       = Resolv::DNS::Resource::IN
UPSTREAM = RubyDNS::Resolver.new([
  [:tcp, '8.8.8.8', 53],
  [:udp, '8.8.8.8', 53]
])

def self.run
  dns = RubyDNS::RuleBasedServer.new do
    match("instance-data", IN::A) do |tx|
      tx.respond!('127.0.0.1')
    end

    otherwise do |tx|
      tx.passthrough!(UPSTREAM)
    end
  end

  files = Rack::Builder.new do
    run Rack::Directory.new(File.expand_path(File.dirname(__FILE__)))
  end

  stop = Proc.new do
    dns.fire(:stop)
    EventMachine.stop
  end

  Signal.trap('INT', &stop)
  Signal.trap('TERM', &stop)

  EventMachine.run do
    puts 'Redirecting http://instance-data to localhost'
    dns.run(:listen => [
      [:tcp, '0.0.0.0', 53],
      [:udp, '0.0.0.0', 53]
    ])

    puts 'Serving metadata on http://169.254.169.254/latest'
    Rack::Handler::Thin.run(files, :Port => PORT)
  end
end

if ENV['USER'] == 'root'
  run
else
  puts 'Error: Please re-run using sudo due to DNS resolver and port 80 usage.'
  exit(1)
end
