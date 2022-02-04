clear all
activity = readtable('8/activity.csv');

activity = activity(1:end-3,:) %NEED THIS LINE FOR PART 1, NOT PART 2s
%%
act_on = table2array((activity(:,1)))./1000;
act_off = table2array((activity(:,2)))./1000;

act_on = act_on - act_on(1);
act_off = act_off - act_on(1);


act_char = table2array(activity(:,3));
act = NaN(size(act_char));

for i = 1:numel(act_char)
    switch char(act_char(i))
        case 'u'
            act(i) = 1;
        case 'w'
            act(i) = 2;
        case 'p'
            act(i) = 3;
        case 'c'
            act(i) = 4;
        case 'hw'
            act(i) = 5;
        case 'hs'
            act(i) = 6;
        case 'ss'
            act(i) = 7;
        case 'sc'
            act(i) = 8;
        case 'sr'
            act(i) = 9;
        case 'l'
            act(i) = 10;
        case '.'
            act(i) = 99;
    end
end
 
%%

class = NaN(size(0:1/50:(91*60)));
class_act = timeseries(class', 0:1/50:(91*60));

for i = 1:length(class)
    if class_act.Time(i) < act_off(end-1)
        if not(isempty(act(find(class_act.Time(i) > act_on, 1, 'last'))))
            ind = find(class_act.Time(i) > act_on, 1, 'last');
            if class_act.Time(i) < act_off(ind)
                class_act.Data(i) = act(ind);
            end
        end
    end
end
%%

% Feature extraction
varnames = {'time','class'}
ds = table(class_act.Time, reshape(class_act.Data, [273001, 1]) , 'VariableNames',varnames);

%%
starti = 1;
stopi = height(ds); %Everything before a sensor dropped out

w = 100

ds_out = table();
indices = 1:w/4:height(ds)-(w+1);

for i = 1:length(indices)
    clear ds_n
    ds_n = table();
    home
    sprintf('Sample %d of %d',i,length(indices))
    ds_t = ds(indices(i):indices(i)+w,:);
    
    
    ds_n.time = mean(ds_t.time);
    ds_n.class = mode(ds_t.class);
   
    ds_out = vertcat(ds_out, ds_n);
    
end

writetable(ds_out,strcat('8/activity_pt1.txt'))

%%
%writetable(ds_out,strcat(dir,'classification',num2str(w),'training.txt'))
