close all
clear variables
clc

%% Directory where the files (ROMS files) are stored
dirpath  = 'D:/ROMS_SILUMATIONS/rsodi1/';
ver_lev  = [-1 -5 -10 -15 -20 -25 -30 -35 -40 -45 -50 -60 -70 -80 -90 -100]; % Niveles verticales (Z) para interpolar
N        = 32; % Numero de niveles RHO
var_name = 'u'; % Nombre de la variable a interpolar en Z

%% Create a new directory to store interpolated variables
mkdir([dirpath , 'interpolatedYearMonth']);
outpath  = [dirpath, 'interpolatedYearMonth/'];
out_name = var_name;

%% Get Interpolated variables each year & month
for year = 1978:2008
    for month = 1:12

    % Read ROMS-PISCES file
    %nc = [dirpath, 'roms_avg_Y', num2str(year), 'M', num2str(month), '.Jaard10kmClim.nc'];
    nc = [dirpath, 'roms6b_avg.Y', num2str(year), '.M', num2str(month), '.rsodi1.nc'];
    disp (['Reading... ' nc '  variable:  ' var_name]); % Display current nc file name
    ncload(nc, 'time_step', 'h', var_name);

    var_name    = eval(var_name);

    vtransform  = 1;
    zDepths     = zlevs(h,0*h,6,0,10,N,'r',vtransform);
    time_step   = time_step(:,4);

    newvar = zeros(size(var_name, 1), length(ver_lev), size(h, 1), size(h, 2));

        for ii = 1:length(time_step)
            var_sub = squeeze(var_name(ii,:,:,:));

            if out_name == 'u'
                for jj = 1:length(ver_lev)
                var_sub = u2rho_3d(var_sub);
                newvar(ii,jj,:,:) = sigmatoz(var_sub,zDepths,ver_lev(jj));
                end
            elseif out_name == 'v'
                for jj = 1:length(ver_lev)
                var_sub = v2rho_3d(var_sub);
                newvar(ii,jj,:,:) = sigmatoz(var_sub,zDepths,ver_lev(jj));
                end
            else
                for jj = 1:length(ver_lev)
                newvar(ii,jj,:,:) = sigmatoz(var_sub,zDepths,ver_lev(jj));
                end
            end
        end

    save([outpath out_name 'Y' num2str(year) 'M' num2str(month) '.mat'],'newvar');
    var_name = out_name;
    end
end

out = table(ver_lev', 'VariableNames', {'depth'});
writetable(out, strcat(outpath, 'depth.txt'))
