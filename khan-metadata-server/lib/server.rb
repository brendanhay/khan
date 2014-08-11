require 'rubygems'
require 'bundler/setup'
require 'eventmachine'
require 'rack'
require 'rubydns'
require 'thin'

require_relative 'rack/metadata'

def self.command(cmd)
  out = `#{cmd}`

  if $? == 0
    out
  else
    raise "Failed to run: #{cmd}"
  end
end

BIND     = '169.254.169.254'
IN       = Resolv::DNS::Resource::IN
UPSTREAM = RubyDNS::Resolver.new([
  [:tcp, '8.8.8.8', 53],
  [:udp, '8.8.8.8', 53]
])

DEVICE =
  if ARGV.length > 0
    ARGV.first.chomp
  else
    puts 'Usage: server.sh <NETWORK_SERVICE> (use networksetup -listallnetworkservices to obtain.)'
    exit 1
  end

OS =
  begin
    host_os = RbConfig::CONFIG['host_os']

    case host_os
    when /darwin|mac os/
      :macosx
    when /linux/
      :linux
    else
      raise "unsupported os: #{host_os.inspect}"
    end
  end

RESOLV =
  if OS == :macosx
    out = command("networksetup -getdnsservers #{DEVICE}")
    command("sudo networksetup -setdnsservers #{DEVICE} 127.0.0.1 8.8.8.8")

    out.gsub("\n", ' ')
  else
    ''
  end

def self.run
  begin
    puts "Adding #{BIND} to lo0"
    command("ifconfig lo0 alias #{BIND}")

    dns = RubyDNS::RuleBasedServer.new do
      match('instance-data', IN::A) do |tx|
        tx.respond!('169.254.169.254')
      end

      otherwise do |tx|
        tx.passthrough!(UPSTREAM)
      end
    end

    dir = Rack::Builder.new do
       root = File.join(File.expand_path(File.dirname(__FILE__)), '../www')
      run Rack::Metadata.new(root)
    end
    
    files = Rack::Chunked.new(Rack::ContentLength.new(dir))

    Signal.trap('INT')  { EventMachine.stop }
    Signal.trap('TERM') { EventMachine.stop }

    EventMachine.run do
      puts 'Redirecting http://instance-data to localhost'
      dns.run(:listen => [
        [:tcp, '0.0.0.0', 53],
        [:udp, '0.0.0.0', 53]
      ])

      puts "Serving metadata on http://#{BIND}/latest"
      Thin::Server.start(BIND, 80, files, :signals => false)
    end
  ensure
    puts "Removing #{BIND} from lo0"
    system("ifconfig lo0 -alias #{BIND}")

    unless RESOLV.empty?
      puts "Reverting resolv.conf to #{RESOLV}"
      system("sudo networksetup -setdnsservers #{DEVICE} #{RESOLV}")
    end
  end
end

if ENV['USER'] == 'root'
  run
else
  puts 'Error: Please re-run using sudo due to resolv.conf modifications and use of port 80.'
  exit 1
end
