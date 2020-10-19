close all
clear variables
clc

%% Directory where the files (ROMS files) are stored
dirpath  = 'E:/ROMS_SILUMATIONS/10kmparent/';
ver_lev  = [-1 -5 -10 -15 -20 -25 -30 -35 -40 -45 -50 -60 -70 -80 -90 -100]; % Niveles verticales (Z) para interpolar
N        = 64; % Numero de niveles RHO
var_name = 'v'; % Nombre de la variable a interpolar

%% Create a new directory to store interpolated variables
mkdir([dirpath , 'interpolated']);
outpath  = [dirpath, 'interpolated/'];
out_name = var_name;

%% Get Interpolated variables each month
for month = 1:12
    % Read ROMS-PISCES file
    nc = [dirpath,'roms_avg_Y2012M', num2str(month),'.Jaard10kmClim.nc'];
    disp (['Reading ... ' nc]); % Display current nc file name
    ncload(nc, 'time_step', 'h', var_name);

    var_name    = eval(var_name);

    vtransform  = 1; 
    zDepths     = zlevs(h,0*h,6,0,10,N,'r',vtransform);
    time_step   = time_step(:,4);

    newvar = zeros(size(var_name, 1), length(ver_lev), size(var_name, 3), size(var_name, 4));
    for ii = 1:length(time_step)
    time_sub = squeeze(var_name(ii,:,:,:));
    
        for jj = 1:length(ver_lev)
        newvar(ii,jj,:,:) = sigmatoz(time_sub,zDepths,ver_lev(jj));
        end
    end

    save([outpath out_name 'M' num2str(month) '.mat'],'newvar');
    var_name = out_name;
end

out = table(ver_lev', 'VariableNames', {'depth'});
writetable(out, strcat(outpath, 'depth.txt'))
