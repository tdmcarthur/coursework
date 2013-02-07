%%path(path,'C:\Users\tdmcarthur\Documents\MATLAB\') ;urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/c_50_00q_v2.xls','temp.xls');[full_data,varnames,raw]=xlsread('temp.xls');orig_obs = size(full_data,1);lagged_data = horzcat(full_data(5:orig_obs, strcmp(varnames,'Year')), ...    full_data(5:orig_obs, strcmp(varnames,'QTR')), ...    full_data(5:orig_obs, strcmp(varnames,'REALGDP')), ...    full_data(1:(orig_obs-4), strcmp(varnames,'CONS')), ...    full_data(4:(orig_obs-1), strcmp(varnames,'UNEMP')) ...    );grid_vec = -2:0.01:2 ;% We will iterate over each value -1 to 2, in steps of 0.01, for each % parametersearch_mat = ones(size(grid_vec, 2), size(grid_vec, 2) );% Create the matrix to fill with SSE valuesfor m=1:size(grid_vec,2)     for n=1:size(grid_vec,2)        model_temp =gdp_model_fn(lagged_data(:, strcmp(varnames,'CONS')), ...          lagged_data(:, strcmp(varnames,'UNEMP')), ...          [grid_vec(m); grid_vec(n)] ) ;        % Calling our model function that produces the y-hats        search_mat(m,n) = ssefn( ...          lagged_data(:, strcmp(varnames,'REALGDP')), model_temp ...        ) ;        % Assigning the results of the sse function to the appropriate        % place in the matrix    endend% 2 Nested loops  since we are filling a m x n matriximagesc(log(search_mat))% Useful check on the shape of the SSE values for the modelmesh(grid_vec,grid_vec, log(search_mat))min_grid_sse = min(min(search_mat));% This will give us the minmin_grid_sol = [ 1 ; 1];% Simply a placeholder [min_grid_sol(1), min_grid_sol(2)] = find(min_grid_sse==search_mat);% Gives us the indices of the corrent row and columnmin_grid_sol(1) = grid_vec(min_grid_sol(1));min_grid_sol(2) = grid_vec(min_grid_sol(2));% Now assigning the actual value of the parameters from the grid vectorfile_ID=fopen('Assignment 1 output.txt', 'a+')parnames = {'B_c' 'B_u', 'SSE', 'Error var.'}estimates =[min_grid_sol(1); min_grid_sol(2); ...    min_grid_sse; min_grid_sse/(length(model_temp)-2)]header = {'Grid Meth.' 'Est.'}partable(parnames,estimates, header);[T, output] = evalc('partable(parnames,estimates, header)');fwrite(file_ID, T)% Printing the results using partable() function and saving to filestep_length = 0.00001% Setting base step lengthmin_gs_sol = min_grid_sol ;% We'll be changing the beta values, so we'll assign the "minimums" from% the grid search to a new variablemin_gs_sse_prev = min_grid_sse ;% Need to have a starting value for the previous SSE so we can compare it% to see if we have a precise enough estimatesse_improv_big = 1e+100sse_improv_big_prev = 1e+100sse_improv_big_current = 1e+100% Set very large values as placeholdersiter = 0% We'll want to know how many "inner" iterations it does to see how% efficient the code iswhile sse_improv_big > 0.01% Must have an improvement in the sse less than 0.01 to get a final result    param_step_factors = eye(2) ;    % We will be clever by creating an identity matrix containing our step    % factors    for i=1:2    % The for loop is for each of the two betas        while (abs(param_step_factors(i, i))>.001)          % The step factor has to be less than .001 to break out of the          % loop. So the change in a given beta will be less than          % 0.00001*.001 = 1e-08                      model_temp = gdp_model_fn(lagged_data(:, strcmp(varnames,'CONS')), ...            lagged_data(:, strcmp(varnames,'UNEMP')), ...            [min_gs_sol(1)+step_length*param_step_factors(1, i); ...            min_gs_sol(2)+step_length*param_step_factors(2, i)] ) ;          % if i=1, then the param_step_factors(1, i) has a positive value          % between 0 and 1, while param_step_factors(2, i) is zero. This          % is how we control which parameter is being changed.                   min_gs_sse_current = ssefn( ...            lagged_data(:, strcmp(varnames,'REALGDP')), model_temp ...          ) ;                    if min_gs_sse_current < min_gs_sse_prev              sse_improv = min_gs_sse_prev-min_gs_sse_current ;          else               param_step_factors(i, i) = param_step_factors(i, i)*(-.8) ;          end          % If we get an improvement in the SSE, we obtain the improvement.          % If not, we switch directions and reduce our step length by 20%          min_gs_sol(i) = min_gs_sol(i)+step_length*param_step_factors(i, i) ;          % Assign the "new" value of the parameter to use as a starting          % point next time          min_gs_sse_prev = min_gs_sse_current ;          % assigned current value of SSE to prev so we can compare it in          % the next iteration                    iter = iter+1 ;        end    end    sse_improv_big_current = min_gs_sse_current ;    sse_improv_big = sse_improv_big_prev - sse_improv_big_current ;    display(sse_improv_big)     sse_improv_big_prev = sse_improv_big_current ;    % This code is just to compute the improvment in the SSE after we have    % refined the betasend parnames = {'B_c' 'B_u', 'SSE', 'Error var.'}estimates =[min_gs_sol(1); min_gs_sol(2); ...     min_gs_sse_current;  min_gs_sse_current/(length(model_temp)-2)]header = {'Gen. Meth.' 'Est.'}partable(parnames,estimates, header);[T, output] = evalc('partable(parnames,estimates, header)');fwrite(file_ID, T)% Printing the results using partable() function and saving to file      %%%%%%   grid_vec_b_c = -2:0.01:2 ;grid_vec_b_u = -2:0.01:2 ;% We will iterate over each value -1 to 2, in steps of 0.01, for each % parameterfinal_search_mat = ones(size(grid_vec_b_c, 2), size(grid_vec_b_u, 2) );% Create the matrix to fill with SSE valuesfor m=1:size(grid_vec_b_c,2)     for n=1:size(grid_vec_b_u,2)         model_temp = gdp_model_fn(lagged_data(:, strcmp(varnames,'CONS')), ...            lagged_data(:, strcmp(varnames,'UNEMP')), ...            [grid_vec_b_c(m); grid_vec_b_u(n)] ) ;                    final_search_mat(m,n) = ml_fn(lagged_data(:, strcmp(varnames,'REALGDP')), ...              model_temp, length(model_temp)-2) ;        % Assigning the results of the sse function to the appropriate        % place in the matrix    endendmin_grid_ml = max(max(final_search_mat));% This will give us the maxmin_grid_sol = [ 1 ; 1];% Simply a placeholder [min_grid_sol(1), min_grid_sol(2)] = find(min_grid_ml==final_search_mat);% Gives us the indices of the corrent row and columnmin_grid_sol(1) = grid_vec(min_grid_sol(1));min_grid_sol(2) = grid_vec(min_grid_sol(2));% Now assigning the actual value of the parameters from the grid vectorparnames = {'B_c' 'B_u', 'ML value'}estimates =[min_grid_sol(1); min_grid_sol(2); ...     min_grid_ml]header = {'ML Grid' 'Est.'}partable(parnames,estimates, header);[T, output] = evalc('partable(parnames,estimates, header)');fwrite(file_ID, T)    step_length = 0.00001% Setting base step lengthml_sol = min_grid_sol ;% We'll be changing the beta values, so we'll assign the "minimums" from% the grid search to a new variableml_value_prev = min_grid_sse ;% Need to have a starting value for the previous SSE so we can compare it% to see if we have a precise enough estimateml_improv_big = 1e+100;ml_improv_big_prev = 1e+100;ml_improv_big_current = 1e+100;% Set very large values as placeholdersiter = 0;% We'll want to know how many "inner" iterations it does to see how% efficient the code iswhile ml_improv_big > 0.0000001% Must have an improvement in the sse less than 0.01 to get a final result    param_step_factors = eye(2) ;    % We will be clever by creating an identity matrix containing our step    % factors    for i=1:2    % The for loop is for each of the two betas        while (abs(param_step_factors(i, i))>.001)          % The step factor has to be less than .001 to break out of the          % loop. So the change in a given beta will be less than          % 0.00001*.001 = 1e-08                      model_temp = gdp_model_fn(lagged_data(:, strcmp(varnames,'CONS')), ...            lagged_data(:, strcmp(varnames,'UNEMP')), ...            [ml_sol(1)+step_length*param_step_factors(1, i); ...            ml_sol(2)+step_length*param_step_factors(2, i)] ) ;          % if i=1, then the param_step_factors(1, i) has a positive value          % between 0 and 1, while param_step_factors(2, i) is zero. This          % is how we control which parameter is being changed.                    ml_value_current = ml_fn(lagged_data(:, strcmp(varnames,'REALGDP')), ...              model_temp, length(model_temp)-2) ;                    if ml_value_current > ml_value_prev              ml_improv = ml_value_current - ml_value_prev ;          else               param_step_factors(i, i) = param_step_factors(i, i)*(-.8) ;          end          % If we get an improvement in the SSE, we obtain the improvement.          % If not, we switch directions and reduce our step length by 20%          ml_sol(i) = ml_sol(i)+step_length*param_step_factors(i, i) ;          % Assign the "new" value of the parameter to use as a starting          % point next time          ml_value_prev = ml_value_current ;          % assigned current value of SSE to prev so we can compare it in          % the next iteration                    iter = iter+1 ;        end    end    ml_improv_big_current = ml_value_current ;    ml_improv_big =  abs(ml_improv_big_current - ml_improv_big_prev );    display(ml_improv_big)     ml_improv_big_prev = ml_improv_big_current ;    % This code is just to compute the improvment in the SSE after we have    % refined the betasendparnames = {'B_c' 'B_u', 'ML value'}estimates =[min_gs_sol(1); min_gs_sol(2); ...     ml_value_current]header = {'ML Meth.' 'Est.'}partable(parnames,estimates, header);[T, output] = evalc('partable(parnames,estimates, header)');fwrite(file_ID, T)% Printing the results using partable() function and saving to file  grid_vec_b_c = (ml_sol(1)-0.001):0.0001:(ml_sol(1)+0.001) ;grid_vec_b_u = (ml_sol(2)-0.001):0.0001:(ml_sol(2)+0.001) ;% We will iterate over each value -1 to 2, in steps of 0.01, for each % parameterfinal_search_mat = ones(size(grid_vec_b_c, 2), size(grid_vec_b_u, 2) );% Create the matrix to fill with SSE valuesfor m=1:size(grid_vec_b_c,2)     for n=1:size(grid_vec_b_u,2)         model_temp = gdp_model_fn(lagged_data(:, strcmp(varnames,'CONS')), ...            lagged_data(:, strcmp(varnames,'UNEMP')), ...            [grid_vec_b_c(m); grid_vec_b_u(n)] ) ;                    final_search_mat(m,n) = ml_fn(lagged_data(:, strcmp(varnames,'REALGDP')), ...              model_temp, length(model_temp)-2) ;        % Assigning the results of the sse function to the appropriate        % place in the matrix    endend% 2 Nested loops  since we are filling a m x n matrix%imagesc(log(final_search_mat))% Useful check on the shape of the SSE values for the modelmesh(grid_vec_b_c, grid_vec_b_u, final_search_mat, zeros(size(final_search_mat)))xlabel('Beta-C')ylabel('Beta-U')zlabel('Ln Likelihood')% Creating the mesh plotprint('-dpng', 'Assignment 1 mesh plot.png')  %%%%%%%%