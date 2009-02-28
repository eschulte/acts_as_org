require 'active_file'
require File.join(File.dirname(__FILE__), 'lib', 'acts_as_org') 
ActiveFile::Base.send(:include, ActiveFile::Acts::Org)

## maybe something here to start up an emacs-client
