function ret = ces_mp_k( betas, x)
  ret = betas(1) * betas(2) * betas(4) * x(1)^(-betas(3)-1) * ...
 (betas(2) * x(1)^(-betas(3))-(betas(2)-1) * x(2)^(-betas(3)))^(-(betas(4)+betas(3))/betas(3));

end