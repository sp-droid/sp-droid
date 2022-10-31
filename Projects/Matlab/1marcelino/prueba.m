%Accelerator & elevator bounds
control_ub = [1.1;0.5];
control_lb = [0.01;-0.5];

%Flight condition
fc = [21;0;0;0];
%Reference or objective
refer = 20;

Ki = [0;0];
Kd = [0;0];
maxk = [0;0];

%% Determine pseudomax value of K
%Accelerator
step = 0.1;
for i=0:step:1000
    Kp = [i;0];
    
    sim('UAV2018NLObs')
    value = max(control);
    
    if value(1)==control_ub(1)
        maxk(1) = i;
        break
    end
end
%Elevator
step = 0.1;
for i=0:step:1000
    Kp = [0;i];
    
    sim('UAV2018NLObs')
    value = max(control);
    
    if value(2)==control_ub(2)
        maxk(2) = i;
        break
    end
end

%% Determine best value for K using genetic algorithms
%Input parameters
nvars = 6; %Accelerator and elevator x 3
lb = [0;0;0;0;0;0]; %Lower bound for K, the upper bound is maxk
ub = [maxk;maxk;maxk];
options = optimoptions('ga','MaxGenerations',10,'PopulationSize',50,'PlotFcn','gaplotbestf');

%
[x,Fval,exitFlag,Output] = ga(@simulation,nvars,[],[],[],[],lb,ub,[],options)


total_error

%% Function
function Y = simulation(K)
    %Accelerator & elevator bounds
    control_ub = [1.1;0.5];
    control_lb = [0.01;-0.5];

    %Flight condition
    fc = [21;0;0;0];
    %Reference or objective
    refer = 20;

    options = simset('SrcWorkspace','current');

    K = K';
    Kp = K(1:2);
    Ki = K(3:4);
    Kd = K(5:6);
    sim('UAV2018NLObs',[],options)
    Y = total_error;
end
