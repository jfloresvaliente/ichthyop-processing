%% OBTENER CLIMATOLOGIAS
varclim = zeros(12, dim2, dim3, dim4);

for ii = 1:12
    var = zeros(size(year,2), dim2, dim3, dim4);
    for jj = 1:size(year,2)
        nc_file = [path,prenom1,num2str(year(jj)),prenom2,num2str(ii),prenom3];
        disp(nc_file)
        ncload (nc_file, 'w');
        var(jj,:,:,:) = squeeze(mean(w,1));
    end
    varclim(ii,:,:,:) = squeeze(mean(var,1));
end

%% INTERPOLAR EN NIVELES Z
zl = zlevs(h,0*h,6,0,10,N,'r',vtransform);
zvar = zeros(12, size(ZM,2), dim3, dim4);

for ii = 1:12
    for jj = 1:size(ZM,2)
    zvar(ii,jj,:,:) = sigmatoz(squeeze(varclim(ii,:,:,:)), zl, ZM(jj));
    end
end

%% GUARDAR ARCHIVO EN FORMATO .MAT
save([path 'wz.mat'],'zvar');
clear 'zvar' 'varsub' 'varclim' 'var' 'w'