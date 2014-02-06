module Rack
  class Metadata
    attr_reader :files

    attr_accessor :root, :path

    def initialize(root, app = nil)
      @root = ::File.expand_path(root)
      @app  = app || Rack::File.new(@root)
    end

    def call(env)
      dup._call(env)
    end

    def _call(env)
      @env         = env
      @script_name = env['SCRIPT_NAME']
      @path_info   = Utils.unescape(env['PATH_INFO'])
      @path        = ::File.join(@root, @path_info)

      @stat = ::File.stat(@path)

      if @stat.readable?
        return @app.call(@env) if @stat.file?
        return list_directory if @stat.directory?
      else
        raise Errno::ENOENT, 'No such file or directory'
      end
    rescue Errno::ENOENT, Errno::ELOOP
      return entity_not_found
    end

    def list_directory
      @files = []

      Dir[::File.join(@path, '*')].sort.each do |f|
        @files << [::File.basename(f)]
      end

      [200, {'Content-Type' => 'plain/text; charset=utf-8'}, self]
    end

    def each
      @files.join("\n").each_line { |l| yield l }
    end

  end
end
