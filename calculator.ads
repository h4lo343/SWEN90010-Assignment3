with PIN;
with Variablestore;
with SimpleStack;

package Calculator with SPARK_Mode  is

	package Stack is new SimpleStack(512, Integer, 0);

    type Calculator is record
        DB : Variablestore.Database;
        MasterPin : PIN.PIN;
        OperandStack : Stack.SimpleStack;
        Lock_State : Boolean;
    end record;

    procedure Init(C: out Calculator; P: in PIN.PIN);

    procedure Unlock(C: in out Calculator; P: in PIN.PIN);

    procedure Lock(C: in out Calculator; P: in PIN.PIN);

end calculator;
