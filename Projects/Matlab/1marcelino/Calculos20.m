% Vt alpha theta q  
X0=[12;0;0;0];
U0=[0.35;0];
Y0=[0];
IX=[1  4];
IU=[];
IY=[1];
[Xtrim,Utrim,Ytrim,DXtrim]=trim('UAV2020Trimar',X0,U0,Y0,IX,IU,IY)

U=Xtrim(1)*cos(Xtrim(2));	% Componentes de la velocidad en ejes cuerpo
W=Xtrim(1)*sin(Xtrim(2));
XDOT=cos(Xtrim(3))*U+sin(Xtrim(3))*W;
ZDOT=sin(Xtrim(3))*U-cos(Xtrim(3))*W;
disp("Velocidad ascensional")
disp(ZDOT*3*60)

%% Comportamiento frente a perturbaciones
K=zeros(2,4);
u0=[0;0];

x0=[-1;0;0;0];
sim('UAV2018NLObs')
figure(1)


x0=[0;0;0.05;0];
sim('UAV2018NLObs')
figure(2)


%% Respuesta al mando
K=zeros(2,4);
x0=[0;0;0;0];

u0=[0.05;0];
sim('UAV2018NLObs')
figure(1)


u0=[0;0.01];
sim('UAV2018NLObs')
figure(2)


%% Controlabilidad

rank(ctrb(A,B))

p=[-8+7i -8-7i -3.5+1i -3.5-1i]*0.8;
K=place(A,B,p);


u0=[0;0];

x0=[-1;0;0.05;0];
sim('UAV2018NLObs',[0,5])
figure(1)

figure(2)



%% LQR
Q=blkdiag(10,1,10,1);
R=blkdiag(800,200)*0.1;
K=lqr(A,B,Q,R)
u0=[0;0];
x0=[-1;0;0.05;0];
sim('UAV2018NLObs',[0,5])
figure(1)

figure(2)




%% Tracking velocidad

Ct=[1 0 0 0];
At=[A zeros(4,1);-Ct 0];
Bt=[B;0 0];
Qt=blkdiag(100,100,100,100,50);
Rt=blkdiag(10,1);
Kt=lqr(At,Bt,Qt,Rt);

K=Kt(:,1:4);
Kt=Kt(:,5);

x0=[0;0;0;0];
sim('UAV2018NLObs')
figure(1)

figure(2)





%% Observador

Co=[1 0 0 0; 0 0 1 0; 0 0 0 1];
rank(obsv(A,Co))

q=eig(A-B*K);
L=place(A',Co',q);
L=L';

x0=[-1;0;0.05;0];
sim('UAV2018NLObs')
figure(1)

figure(2)

