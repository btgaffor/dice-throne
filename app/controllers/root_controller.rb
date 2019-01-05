class RootController < ApplicationController
  def index
    respond_to :html
  end

  def save
    puts 'VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV'
    puts ''
    puts 'SAVING'
    puts params.to_unsafe_h
    puts ''
    puts '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'
    ActionCable.server.broadcast('game_first-game', data: 42)
    head :ok
  end
end
