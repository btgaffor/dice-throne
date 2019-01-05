Rails.application.routes.draw do
  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html
  root 'root#index'
  post '/save', to: 'root#save'

  # mount ActionCable.server, at: '/cable'
end
