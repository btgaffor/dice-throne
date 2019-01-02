class RootController < ApplicationController
  def index
    respond_to :html
  end

  def save
    puts 'VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV'
    puts ''
    puts 'SAVING'
    puts ''
    puts '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
    ActionCable.server.broadcast("game_first-game", data: 42)
    render plain: 'hi'
  end
end
