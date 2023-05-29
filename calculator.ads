with PIN;
with Variablestore;
with SimpleStack;

package Calculator with SPARK_Mode  is

	package Stack is new SimpleStack(512, Integer, 0);
	Incorrect_Pin: String := "The Pin Entered Is Incorrect";
	Divide_By_Zero: String := "Can not divide by 0, Calculator Will Exit Now";
	Overflow_Occur: String := "Overflow occured in the operation";

    type Calculator is record
        DB : Variablestore.Database;
        MasterPin : PIN.PIN;
        OperandStack : Stack.SimpleStack;
        Lock_State : Boolean;
    end record;

    procedure Init(C: out Calculator; P: in PIN.PIN);

    procedure Unlock(C: in out Calculator; P: in PIN.PIN);

    procedure Lock(C: in out Calculator; P: in PIN.PIN);

    procedure Push(C: in out Calculator; N: in Integer);

    procedure Store(C: in out Calculator; V: VariableStore.Variable);

    procedure Remove(C: in out Calculator; V: VariableStore.Variable);

    procedure Pop(C: in out Calculator);

    procedure Load(C: in out Calculator; V: VariableStore.Variable);

    procedure List(C: in Calculator);

    procedure Add(C: in out Calculator);

    procedure Minus(C: in out Calculator);

    procedure Multiply(C: in out Calculator);

    procedure Divide(C: in out Calculator);

    function StackSize(C: in Calculator) return Integer;

    private

       function StackSize(C: in Calculator) return Integer is 
       (Stack.Size(C.OperandStack));

end calculator;
