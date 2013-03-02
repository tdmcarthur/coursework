function ret = gas_likelihood_fn(betas, data_mat)
  % Betas must be a k x 1 vector
  BETA  = vertcat(betas(1:9), repmat(0, 5, 1));
  ALPHA = vertcat(repmat(0, 9, 1), betas(10:length(betas)) ) ;
  
  ret =  sum( ...
    -(1/2)*log(2*pi) ...
    -(1/2) .* data_mat(:, 2:size(data_mat,2)) * ALPHA ...
    -(1/2) .* exp( - data_mat(:, 2:size(data_mat,2)) * ALPHA) .* ...
    ( data_mat(:, 1) - data_mat(:, 2:size(data_mat,2)) * BETA ) .^2 ...
    );  
  
end