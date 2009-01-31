module ActiveFile
  module Acts
    module Org
      EXP_PREFIX = ".exported_"
      
      def self.included(base)
        base.extend ActiveFile::Acts::Org::ClassMethods
      end

      module ClassMethods
        def acts_as_org(options={})
          return if self.included_modules.include?(ActiveFile::Acts::Org::InstanceMethods)
          send(:include, ActiveFile::Acts::Org::InstanceMethods)
        end
        
        def emacs_run(command)
          %x{emacs -Q -batch -l #{File.join(File.dirname(__FILE__), "..", "elisp", "org-interaction.el")} -eval '#{command}'}
        end
      end
      
      module InstanceMethods
        def html_path
          File.join(File.dirname(self.full_path),
                    ActiveFile::Acts::Org::EXP_PREFIX + File.basename(self.path))
        end
        
        def clean_html?
          File.exist?(self.html_path) and File.mtime(self.html_path) > File.mtime(self.full_path)
        end

        def to_html(options = {})
          options = {:postamble => false}.merge(options)
          self.class.emacs_run "(org-file-to-html  \"#{self.full_path}\")" unless self.clean_html?
          return nil unless File.exist?(self.html_path)
          html = File.read(self.html_path)
          # extract the body portion
          start_body = (html =~ /<body>/) + 6
          end_body = (html =~ /<\/body>/) - 1
          body = html[(start_body..end_body)]
          if options[:postamble]
            body
          else
            body[(0..(body.index("<div id=\"postamble\">") - 1))]
          end
        end
      end
    end
  end
end
