require 'rubygems'
require 'bundler/setup'
require 'eventmachine'
require 'rack'
require 'rubydns'
require 'thin'

BIND     = '169.254.169.254'
IN       = Resolv::DNS::Resource::IN
UPSTREAM = RubyDNS::Resolver.new([
  [:tcp, '8.8.8.8', 53],
  [:udp, '8.8.8.8', 53]
])

def self.run
  begin
    puts "Adding #{BIND} to lo0"
    system("ifconfig lo0 alias #{BIND}")


    dns = RubyDNS::RuleBasedServer.new do
      match("instance-data", IN::A) do |tx|
        tx.respond!('127.0.0.1')
      end

      otherwise do |tx|
        tx.passthrough!(UPSTREAM)
      end
    end

    dir = Rack::Builder.new do
      run Rack::Directory.new(File.expand_path(File.dirname(__FILE__)))
    end

    app   = Rack::Chunked.new(Rack::ContentLength.new(dir))
    files = Thin::Server.new(BIND, 80, app)

    Signal.trap('INT', EventMachine.stop)
    Signal.trap('TERM', EventMachine.stop)

    EventMachine.run do
      puts 'Redirecting http://instance-data to localhost'
      dns.run(:listen => [
        [:tcp, '0.0.0.0', 53],
        [:udp, '0.0.0.0', 53]
      ])

      puts "Serving metadata on http://#{BIND}/latest"
      files.start
    end
  ensure
    puts "Removing #{BIND} from lo0"
    system("ifconfig lo0 -alias #{BIND}")
  end
end

if ENV['USER'] == 'root'
  run
else
  puts 'Error: Please re-run using sudo due to resolv.conf and port 80 usage.'
  exit(1)
end
