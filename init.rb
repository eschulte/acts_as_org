require 'active_file'
require 'acts_as_org'
ActiveFile::Base.send(:include, ActiveFile::Acts::Org)

## maybe something here to start up an emacs-client
