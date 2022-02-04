% function output = sync_participant(id, w, smooth_opt)
% 
% if nargin == 1
%     w = 200;
%     smooth_opt = 1;
% elseif nargin == 2
%     smooth_opt = 1;
% end
tic
cd('X:\imu_long')
addpath step1-synchronization
clear all

id = 102;
session = 1;
w = 100;
smooth_opt = 1; 

dir = strcat(num2str(id),'/',num2str(session),'/');

wrist = readtable(strcat(dir,'imu/caregiver/wrist.csv'));
hip = readtable(strcat(dir,'imu/caregiver/hip.csv'));

% rwrist.Var1 = rwrist.Var1 + 28253; %Manual offset
% lwrist.Var1 = lwrist.Var1 + 28253;

%%
%
wtime = posixtime(wrist.Var1);
for i = 1:length(wtime)
    if isnan(wtime(i))
        wtime(i) = wtime(i-1) + .02;
    end
end
htime = posixtime(hip.Var1);
for i = 1:length(htime)
    if isnan(htime(i))
        htime(i) = htime(i-1)  + .02;
    end
end
offset = max([htime(1) wtime(1)]);
%%
acc_w = timeseries([wrist.Var2 wrist.Var3 wrist.Var4], wtime);
acc_h = timeseries([hip.Var2 hip.Var3 hip.Var4], htime);
gyr_w = timeseries([wrist.Var5 wrist.Var6 wrist.Var7], wtime);
gyr_h = timeseries([hip.Var5 hip.Var6 hip.Var7], htime);

%%

new_time = wtime(1):1/50:wtime(end);

acc_w = resample(acc_w,new_time);
acc_h = resample(acc_h,new_time);
gyr_w = resample(gyr_w,new_time);
gyr_h = resample(gyr_h,new_time);


%%

%Hard coded values 
start_time = datetime(1626117158.015,'ConvertFrom','epochtime')- hours(7); %from baby sensors
%first_jump = (1626117158015000-offset)/1000000; %from baby sensors
%nap_on = (1626123609890000-offset)/1000000;
%nap_off = (1626128440659000-offset)/1000000;
%study_end = (1626145211517000-offset)/1000000;

hip_strike = datetime(1626089031.399000,'ConvertFrom','epochtime') + hours(1);
wrist_strike = datetime(1626089032.199000,'ConvertFrom','epochtime') + hours(1);
study_end = datetime(1626116867.899000,'ConvertFrom','epochtime') + hours(1);
%%
activity_complete = readtable(strcat(dir,'coding/activity_parent.csv'));

%%

special = activity_complete(activity_complete.Var3 == "activity" | activity_complete.Var3 == "hip" | activity_complete.Var3 == "wrist" | activity_complete.Var3 == "sync" | activity_complete.Var3 == "freeplay", :);
special.Var1 = (special.Var1-126)/100000000+hip_strike;
special.Var2 = (special.Var2-126)/100000000+hip_strike;

activity = activity_complete(not(activity_complete.Var3 == "activity" | activity_complete.Var3 == "hip" | activity_complete.Var3 == "wrist" | activity_complete.Var3 == "sync" | activity_complete.Var3 == "freeplay"), :);

%%
act_on = (table2array(activity(:,1))-126)./100000000+hip_strike;
act_off = (table2array(activity(:,2))-126)./100000000+hip_strike;

act_char = table2array(activity(:,3));
act = NaN(size(act_char));

for i = 1:numel(act_char)
    switch char(act_char(i))
        case 'u'
            act(i) = 1;
        case 'd'
            act(i) = 2;
        case 's'
            act(i) = 3;
    end
end
            
class = NaN(size(acc_h.Time));
class_act = timeseries(class, acc_h.Time);
%%
for i = 1:length(class)
    t = datetime(class_act.Time(1),'ConvertFrom','epochtime');
    if t < act_off(end-1)
        if not(isempty(act(find(t > act_on, 1, 'last'))))
            ind = find(t > act_on, 1, 'last');
            if t < act_off(ind)
                class_act.Data(i) = act(find(t > act_on, 1, 'last'));
            end
        end
    end
end
%%
plot(acc_w.Time, acc_w.Data(:,1))
hold on
plot(class_act,'r')
%vline(first_jump,'k')
%vline(nap_on,'k')
%vline(nap_off,'k')
vline(study_end,'k')
hold off

%%

% Feature extraction
varnames = {'time', 'x1', 'y1', 'z1', 'x2','y2','z2','x3','y3','z3','x4','y4','z4','x1d', 'y1d', 'z1d', 'x2d','y2d','z2d','x3d','y3d','z3d','x4d','y4d','z4d','class'};

ds = table(acc_h.Time, acc_w.Data(:,1),acc_w.Data(:,2),acc_w.Data(:,3), ...
                        acc_h.Data(:,1),acc_h.Data(:,2),acc_h.Data(:,3),...
                        acc_la.Data(:,1),acc_la.Data(:,2),acc_la.Data(:,3),...
                        acc_lt.Data(:,1),acc_lt.Data(:,2),acc_lt.Data(:,3),...
                        gyr_w.Data(:,1),gyr_w.Data(:,2),gyr_w.Data(:,3),...
                        gyr_h.Data(:,1),gyr_h.Data(:,2),gyr_h.Data(:,3),...
                        gyr_la.Data(:,1),gyr_la.Data(:,2),gyr_la.Data(:,3),...
                        gyr_lt.Data(:,1),gyr_lt.Data(:,2),gyr_lt.Data(:,3),...
                        class_act.Data, 'VariableNames',varnames);

%
ds = ds(ds.time > special.Var1(1),:); %Filter out everything before first activity phase                 
ds = ds(ds.time < study_end,:);

%writetable(ds,strcat(dir,'raw_sensors.txt'))

% %All day data
% starti = find(not(isnan(ds.class)),1,'first');
% stopi = study_end; 
% 
% ds = ds(starti:stopi,:);
ds_out = table();
indices = 1:w/4:height(ds)-(w+1);

%
% Extract features for each time window

%ds_t = the data for each time window pulled from the master
%ds_n = the calculated data that gets stored into the output
%ds_tfft = a temporary dataset to get frequency measures
%ds_out = the dataset that gets appended to vertically with features for
%each sample

parfor i = 1:length(indices)
    ds_n = table();
    home
    sprintf('Sample %d of %d',i,length(indices))
    ds_t = ds(indices(i):indices(i)+w,:);
    
    if smooth_opt == 1
        s = 10;
        ds_t.x1 = smooth(ds_t.x1,s);
        ds_t.y1 = smooth(ds_t.y1,s);
        ds_t.z1 = smooth(ds_t.z1,s);
        ds_t.x2 = smooth(ds_t.x2,s);
        ds_t.y2 = smooth(ds_t.y2,s);
        ds_t.z2 = smooth(ds_t.z2,s);
        ds_t.x3 = smooth(ds_t.x3,s);
        ds_t.y3 = smooth(ds_t.y3,s);
        ds_t.z3 = smooth(ds_t.z3,s);
        ds_t.x4 = smooth(ds_t.x4,s);
        ds_t.y4 = smooth(ds_t.y4,s);
        ds_t.z4 = smooth(ds_t.z4,s);
        ds_t.x1d = smooth(ds_t.x1d,s);
        ds_t.y1d = smooth(ds_t.y1d,s);
        ds_t.z1d = smooth(ds_t.z1d,s);
        ds_t.x2d = smooth(ds_t.x2d,s);
        ds_t.y2d = smooth(ds_t.y2d,s);
        ds_t.z2d = smooth(ds_t.z2d,s);
        ds_t.x3d = smooth(ds_t.x3d,s);
        ds_t.y3d = smooth(ds_t.y3d,s);
        ds_t.z3d = smooth(ds_t.z3d,s);
        ds_t.x4d = smooth(ds_t.x4d,s);
        ds_t.y4d = smooth(ds_t.y4d,s);
        ds_t.z4d = smooth(ds_t.z4d,s);
    elseif smooth_opt == 2
    %FOR THE FUTURE, WRITE THIS AS A 25 Hz 4th order low-pass Butterworth
    %filter like in the Stewart paper
        %[b,a] = butter(4,25/(SAMPLING_RATE/2)));
        %x1_lowpass = filter(b, a, ds.x1);
    end
    
    ds_n.time = mean(ds_t.time);
    ds_n.clock_time = start_time + seconds(mean(ds_t.time) - first_jump);
    ds_n.class = mode(ds_t.class);
    ds_n.class_prop = sum(ds_t.class == ds_n.class)/height(ds_t);
    
    %Acceleration directional sums
    sumx = ds_t.x1 + ds_t.x2 + ds_t.x3 + ds_t.x4;
    sumy = ds_t.y1 + ds_t.y2 + ds_t.y3 + ds_t.y4;
    sumz = ds_t.z1 + ds_t.z2 + ds_t.z3 + ds_t.z4;
    
    ds_n.x_sum = sum(sumx);
    ds_n.y_sum = sum(sumy);
    ds_n.z_sum = sum(sumz);
    
    ds_n.corr_xy = corr(sumx, sumy);
    ds_n.corr_xz = corr(sumx, sumz);
    ds_n.corr_yz = corr(sumy, sumz);
    
    ds_n.diff_xy = mean(sumz-sumx);
    ds_n.diff_xz = mean(sumy-sumx);
    ds_n.diff_yz = mean(sumy-sumz);
    
    %Acceleration sums
    sum1 = ds_t.x1 + ds_t.y1 + ds_t.z1;
    sum2 = ds_t.x2 + ds_t.y2 + ds_t.z2;
    sum3 = ds_t.x3 + ds_t.y3 + ds_t.z3;
    sum4 = ds_t.x4 + ds_t.y4 + ds_t.z4;
    
    ds_n.sum1 = sum(sum1);
    ds_n.sum2 = sum(sum2);
    ds_n.sum3 = sum(sum3);
    ds_n.sum4 = sum(sum4);
    
    ds_n.corr_12 = corr(sum1, sum2);
    ds_n.corr_13 = corr(sum1, sum3);
    ds_n.corr_14 = corr(sum1, sum4);
    ds_n.corr_23 = corr(sum2, sum3);
    ds_n.corr_24 = corr(sum2, sum4);
    ds_n.corr_34 = corr(sum3, sum4);
    
    ds_n.diff_12 = mean(sum2-sum1);
    ds_n.diff_13 = mean(sum3-sum1);
    ds_n.diff_14 = mean(sum4-sum1);
    ds_n.diff_23 = mean(sum3-sum2);
    ds_n.diff_24 = mean(sum4-sum2);
    ds_n.diff_34 = mean(sum4-sum3);
    
    %Acceleration magnitudes
    mag1 = (ds_t.x1.^2 + ds_t.y1.^2 + ds_t.z1.^2).^.5;
    mag2 = (ds_t.x2.^2 + ds_t.y2.^2 + ds_t.z2.^2).^.5;
    mag3 = (ds_t.x3.^2 + ds_t.y3.^2 + ds_t.z3.^2).^.5;
    mag4 = (ds_t.x4.^2 + ds_t.y4.^2 + ds_t.z4.^2).^.5;
    
    ds_n.mag1 = sum(mag1);
    ds_n.mag2 = sum(mag2);
    ds_n.mag3 = sum(mag3);
    ds_n.mag4 = sum(mag4);
    
    ds_n.corrm_12 = corr(mag1, mag2);
    ds_n.corrm_13 = corr(mag1, mag3);
    ds_n.corrm_14 = corr(mag1, mag4);
    ds_n.corrm_23 = corr(mag2, mag3);
    ds_n.corrm_24 = corr(mag2, mag4);
    ds_n.corrm_34 = corr(mag3, mag4);
    
    ds_n.diffm_12 = mean(mag2-mag1);
    ds_n.diffm_13 = mean(mag3-mag1);
    ds_n.diffm_14 = mean(mag4-mag1);
    ds_n.diffm_23 = mean(mag2-mag3);
    ds_n.diffm_24 = mean(mag4-mag2);
    ds_n.diffm_34 = mean(mag4-mag3);
    
   %Gyro directional sums
    sumx = ds_t.x1d + ds_t.x2d + ds_t.x3d + ds_t.x4d;
    sumy = ds_t.y1d + ds_t.y2d + ds_t.y3d + ds_t.y4d;
    sumz = ds_t.z1d + ds_t.z2d + ds_t.z3d + ds_t.z4d;
    
    ds_n.x_sumd = sum(sumx);
    ds_n.y_sumd = sum(sumy);
    ds_n.z_sumd = sum(sumz);
    
    ds_n.corr_xyd = corr(sumx, sumy);
    ds_n.corr_xzd = corr(sumx, sumz);
    ds_n.corr_yzd = corr(sumy, sumz);
    
    ds_n.diff_xyd = mean(sumz-sumx);
    ds_n.diff_xzd = mean(sumy-sumx);
    ds_n.diff_yzd = mean(sumy-sumz);
    
    %Gyro sums
    sum1 = ds_t.x1d + ds_t.y1d + ds_t.z1d;
    sum2 = ds_t.x2d + ds_t.y2d + ds_t.z2d;
    sum3 = ds_t.x3d + ds_t.y3d + ds_t.z3d;
    sum4 = ds_t.x4d + ds_t.y4d + ds_t.z4d;
    
    ds_n.sum1d = sum(sum1);
    ds_n.sum2d = sum(sum2);
    ds_n.sum3d = sum(sum3);
    ds_n.sum4d = sum(sum4);
    
    ds_n.corr_12d = corr(sum1, sum2);
    ds_n.corr_13d = corr(sum1, sum3);
    ds_n.corr_14d = corr(sum1, sum4);
    ds_n.corr_23d = corr(sum2, sum3);
    ds_n.corr_24d = corr(sum2, sum4);
    ds_n.corr_34d = corr(sum3, sum4);
    
    ds_n.diff_12d = mean(sum2-sum1);
    ds_n.diff_13d = mean(sum3-sum1);
    ds_n.diff_14d = mean(sum4-sum1);
    ds_n.diff_23d = mean(sum3-sum2);
    ds_n.diff_24d = mean(sum4-sum2);
    ds_n.diff_34d = mean(sum4-sum3);
    
    %Gyro magnitudes
    mag1 = (ds_t.x1d.^2 + ds_t.y1d.^2 + ds_t.z1d.^2).^.5;
    mag2 = (ds_t.x2d.^2 + ds_t.y2d.^2 + ds_t.z2d.^2).^.5;
    mag3 = (ds_t.x3d.^2 + ds_t.y3d.^2 + ds_t.z3d.^2).^.5;
    mag4 = (ds_t.x4d.^2 + ds_t.y4d.^2 + ds_t.z4d.^2).^.5;
    
    ds_n.mag1d = sum(mag1);
    ds_n.mag2d = sum(mag2);
    ds_n.mag3d = sum(mag3);
    ds_n.mag4d = sum(mag4);
    
    ds_n.corrm_12d = corr(mag1, mag2);
    ds_n.corrm_13d = corr(mag1, mag3);
    ds_n.corrm_14d = corr(mag1, mag4);
    ds_n.corrm_23d = corr(mag2, mag3);
    ds_n.corrm_24d = corr(mag2, mag4);
    ds_n.corrm_34d = corr(mag3, mag4);
    
    ds_n.diffm_12d = mean(mag2-mag1);
    ds_n.diffm_13d = mean(mag3-mag1);
    ds_n.diffm_14d = mean(mag4-mag1);
    ds_n.diffm_23d = mean(mag2-mag3);
    ds_n.diffm_24d = mean(mag4-mag2);
    ds_n.diffm_34d = mean(mag4-mag3);
    
    %from here only using varfun, no more manual stats
    ds_t.time = [];
    %ds_t.clock_time = [];
    ds_t.class = [];
    ds_n = horzcat(ds_n, varfun(@mean, ds_t));
    ds_n = horzcat(ds_n, varfun(@median, ds_t));
    ds_n = horzcat(ds_n, varfun(@std, ds_t));
    ds_n = horzcat(ds_n, varfun(@skewness, ds_t));
    ds_n = horzcat(ds_n, varfun(@kurtosis, ds_t));
    ds_n = horzcat(ds_n, varfun(@per25, ds_t));
    ds_n = horzcat(ds_n, varfun(@per75, ds_t));
    ds_n = horzcat(ds_n, varfun(@min, ds_t));
    ds_n = horzcat(ds_n, varfun(@max, ds_t));
    
    ds_tfft = varfun(@fft, ds_t);
    ds_tfft = varfun(@abs, ds_tfft);
    ds_tfft = ds_tfft(1:w/2+1,:);
    
    ds_n = horzcat(ds_n, varfun(@max, ds_tfft));
    temp = varfun(@(A) find(A == max(A(:))), ds_tfft);
    temp.Properties.VariableNames = {'fft_ind_x1','fft_ind_y1','fft_ind_z1', 'fft_ind_x2','fft_ind_y2','fft_ind_z2','fft_ind_x3','fft_ind_y3','fft_ind_z3','fft_ind_x4','fft_ind_y4','fft_ind_z4','fft_ind_x1d','fft_ind_y1d','fft_ind_z1d', 'fft_ind_x2d','fft_ind_y2d','fft_ind_z2d','fft_ind_x3d','fft_ind_y3d','fft_ind_z3d','fft_ind_x4d','fft_ind_y4d','fft_ind_z4d',};
    ds_n = horzcat(ds_n, temp);
    temp = varfun(@(A) A(1), ds_tfft);
    temp.Properties.VariableNames = {'fft_ind1_x1','fft_ind1_y1','fft_ind1_z1', 'fft_ind1_x2','fft_ind1_y2','fft_ind1_z2','fft_ind1_x3','fft_ind1_y3','fft_ind1_z3','fft_ind1_x4','fft_ind1_y4','fft_ind1_z4','fft_ind1_x1d','fft_ind1_y1d','fft_ind1_z1d', 'fft_ind1_x2d','fft_ind1_y2d','fft_ind1_z2d','fft_ind1_x3d','fft_ind1_y3d','fft_ind1_z3d','fft_ind1_x4d','fft_ind1_y4d','fft_ind1_z4d',};
    ds_n = horzcat(ds_n, temp);
    temp = varfun(@(A) A(2), ds_tfft);
    temp.Properties.VariableNames = {'fft_ind2_x1','fft_ind2_y1','fft_ind2_z1', 'fft_ind2_x2','fft_ind2_y2','fft_ind2_z2','fft_ind2_x3','fft_ind2_y3','fft_ind2_z3','fft_ind2_x4','fft_ind2_y4','fft_ind2_z4','fft_ind2_x1d','fft_ind2_y1d','fft_ind2_z1d', 'fft_ind2_x2d','fft_ind2_y2d','fft_ind2_z2d','fft_ind2_x3d','fft_ind2_y3d','fft_ind2_z3d','fft_ind2_x4d','fft_ind2_y4d','fft_ind2_z4d',};
    ds_n = horzcat(ds_n, temp);
    temp = varfun(@(A) A(3), ds_tfft);
    temp.Properties.VariableNames = {'fft_ind3_x1','fft_ind3_y1','fft_ind3_z1', 'fft_ind3_x2','fft_ind3_y2','fft_ind3_z2','fft_ind3_x3','fft_ind3_y3','fft_ind3_z3','fft_ind3_x4','fft_ind3_y4','fft_ind3_z4','fft_ind3_x1d','fft_ind3_y1d','fft_ind3_z1d', 'fft_ind3_x2d','fft_ind3_y2d','fft_ind3_z2d','fft_ind3_x3d','fft_ind3_y3d','fft_ind3_z3d','fft_ind3_x4d','fft_ind3_y4d','fft_ind3_z4d',};
    ds_n = horzcat(ds_n, temp);
    temp = varfun(@(A) A(4), ds_tfft);
    temp.Properties.VariableNames = {'fft_ind4_x1','fft_ind4_y1','fft_ind4_z1', 'fft_ind4_x2','fft_ind4_y2','fft_ind4_z2','fft_ind4_x3','fft_ind4_y3','fft_ind4_z3','fft_ind4_x4','fft_ind4_y4','fft_ind4_z4','fft_ind4_x1d','fft_ind4_y1d','fft_ind4_z1d', 'fft_ind4_x2d','fft_ind4_y2d','fft_ind4_z2d','fft_ind4_x3d','fft_ind4_y3d','fft_ind4_z3d','fft_ind4_x4d','fft_ind4_y4d','fft_ind4_z4d',};
    ds_n = horzcat(ds_n, temp);
    temp = varfun(@(A) A(1), ds_tfft);
    temp.Properties.VariableNames = {'fft_ind5_x1','fft_ind5_y1','fft_ind5_z1', 'fft_ind5_x2','fft_ind5_y2','fft_ind5_z2','fft_ind5_x3','fft_ind5_y3','fft_ind5_z3','fft_ind5_x4','fft_ind5_y4','fft_ind5_z4','fft_ind5_x1d','fft_ind5_y1d','fft_ind5_z1d', 'fft_ind5_x2d','fft_ind5_y2d','fft_ind5_z2d','fft_ind5_x3d','fft_ind5_y3d','fft_ind5_z3d','fft_ind5_x4d','fft_ind5_y4d','fft_ind5_z4d',};
    ds_n = horzcat(ds_n, temp);

    
    ds_out = vertcat(ds_out, ds_n);
    
end
toc
% add a variables for study events
%video portion
ds_out.video_time = ones(height(ds_out),1);
ds_out.video_time(not(ds_out.time > special.Var2(3))) = 0;

%nap time
ds_out.nap_time = ones(height(ds_out),1) - 1;
ds_out.nap_time(not(ds_out.time > nap_on & ds_out.time < nap_off)) = 1;

%
writetable(ds_out,strcat(dir,'imu/classification',num2str(w),'.txt'))
