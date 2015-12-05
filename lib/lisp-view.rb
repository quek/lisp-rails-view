require "lisp-view/version"

module Lisp
  module View
    class Railtie < ::Rails::Railtie
      initializer :lisp do |app|
        ActiveSupport.on_load :action_view do
          ActionView::Template.register_template_handler :lisp, LispHandler
        end
      end
    end

    class LispHandler

      LISP = File.expand_path('../../lisp/lisp-view.lisp', __FILE__)

      def self.call(template)
        file = template.identifier
        x = `sbcl --script #{LISP} #{file}`
        Rails.logger.debug('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$')
        Rails.logger.debug(x)
        Rails.logger.debug('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$')
        <<EOT
[].tap { |b__|
    #{x}
}.flatten
EOT
      end
    end
  end
end
