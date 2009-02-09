module ActiveFile
  module Acts
    module Org
      # *note*: if you change this value, you must also change the
      # value of `org-interaction-prefix' in
      # ../elisp/org-interaction.el
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
        
        # convert a string of org-formatted text to html
        def string_to_html(org_string, options = {})
          tmp = Tempfile.new("org-string")
          tmp << org_string
          tmp.flush
          self.to_html(tmp.path, options)
        end
        
        def html_path(path)
          File.join(File.dirname(path),
                    ActiveFile::Acts::Org::EXP_PREFIX + File.basename(path))
        end
        
        def to_html(path, options = {})
          h_path = self.html_path(path)
          options = {:postamble => false}.merge(options)
          self.emacs_run("(org-file-to-html  \"#{path}\")") unless self.clean_html?(path)
          return nil unless File.exist?(h_path)
          html = File.read(h_path)
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
        
        def clean_html?(path)
          h_path = self.html_path(path)
          File.exist?(h_path) and File.mtime(h_path) > File.mtime(path)
        end
        
        def latex_path(path)
          File.join(File.dirname(path),
                    ActiveFile::Acts::Org::EXP_PREFIX + File.basename(path) + ".tex")
        end
        
        def to_latex(path, options = {})
          e_path = self.latex_path(path)
          options = {:postamble => false}.merge(options)
          self.emacs_run("(org-file-to-latex  \"#{path}\")") unless self.clean_latex?(path)
          return nil unless File.exist?(e_path)
          html = File.read(e_path)
        end
        
        def clean_latex?(path)
          l_path = self.latex_path(path)
          File.exist?(l_path) and File.mtime(l_path) > File.mtime(path)
        end
      end
      
      module InstanceMethods
        def html_path
          self.class.html_path(self.full_path)
        end
        
        def clean_html?
          self.class.clean_html?(self.full_path)
        end
        
        def to_html(options = {})
          self.class.to_html(self.full_path, options)
        end
        
        def latex_path
          self.class.latex_path(self.full_path)
        end
        
        def clean_latex?
          self.class.clean_latex?(self.full_path)
        end
        
        def to_latex(options = {})
          self.class.to_latex(self.full_path, options)
        end
      end
    end
  end
end
