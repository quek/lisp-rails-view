require "lisp-rails-view/version"

module LispRailsView
  class Railtie < ::Rails::Railtie
    initializer :lisp do |app|
      ActiveSupport.on_load :action_view do
        ActionView::Template.register_template_handler :lisp, LispHandler
      end
    end
  end

  class LispHandler

    LISP = File.expand_path('../../lisp/lisp-rails-view.lisp', __FILE__)

    def self.call(template)
      file = template.identifier
      x = `sbcl --script #{LISP} #{file}`
      code = <<EOT
[].tap do |b__|
def b__.push(x)
  if x.is_a?(Array)
    x.map do |y|
      push(y)
    end
  else
    super(x.html_safe? ? x : ERB::Util.h(x))
  end
end
#{x}
end.flatten
EOT
      Rails.logger.debug('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$')
      Rails.logger.debug(code)
      Rails.logger.debug('$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$')
      code
    end
  end
end
