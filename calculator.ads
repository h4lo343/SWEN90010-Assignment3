with PIN;
use PIN;
with VariableStore;
with SimpleStack;
with Ada.Containers;
use Ada.Containers;

package Calculator with SPARK_Mode  is

    package Stack is new SimpleStack(512, Integer, 0);
    Incorrect_Pin: String := "The Pin Entered Is Incorrect";
    Divide_By_Zero: String := "Can not divide by 0, Calculator Will Exit Now";
    Overflow_Occur: String := "Overflow occured in the operation";

    type Calculator is record
        DB : VariableStore.Database;
        MasterPin : PIN.PIN;
        OperandStack : Stack.SimpleStack;
        Lock_State : Boolean;
    end record;

    procedure Init(C: out Calculator; P: in PIN.PIN);

    procedure Unlock(C: in out Calculator; P: in PIN.PIN) with 
    Pre => C.Lock_State = True;

    procedure Lock(C: in out Calculator; P: in PIN.PIN) with
    Pre => (C.Lock_State = False),
    Post => C.MasterPin = P;

    procedure Push(C: in out Calculator; N: in Integer) with
    Pre => (StackSize(C) < 512 
    	and C.Lock_State = False 
    	and N >= Integer'First 
    	and N <= Integer'Last),
    Post => ((StackSize(C) = StackSize(C'Old) + 1) 
    	and Stack.Storage(C.OperandStack, StackSize(C)) = N);

    procedure Store(C: in out Calculator; V: VariableStore.Variable) with
    Pre => (VariableStore.Length(C.DB) < VariableStore.Max_Entries 
    	and StackSize(C) > 0 
    	and C.Lock_State = False),
    Post => (StackSize(C) = StackSize(C'Old) - 1);

    procedure Remove(C: in out Calculator; V: VariableStore.Variable) with
    Pre => (VariableStore.Has_Variable(C.DB, V) and C.Lock_State = False);

    procedure Pop(C: in out Calculator) with
    Pre => (StackSize(C) > 0 and C.Lock_State = False),
    Post => (StackSize(C) = StackSize(C'Old) - 1);

    procedure Load(C: in out Calculator; V: VariableStore.Variable) with
    Pre => (VariableStore.Has_Variable(C.DB, V) 
    	and StackSize(C) < 512 
    	and C.Lock_State = False),
    Post => (StackSize(C) = StackSize(C'Old) + 1);

    procedure List(C: in Calculator) with
    Pre => (C.Lock_State = False),
    Global => null;

    procedure Add(C: in out Calculator) with
    Pre => (StackSize(C) >= 2 and C.Lock_State = False),
    Post => (StackSize(C) <= StackSize(C'Old));

    procedure Minus(C: in out Calculator) with
    Pre => (StackSize(C) >= 2 and C.Lock_State = False),
    Post => (StackSize(C) <= StackSize(C'Old));

    procedure Multiply(C: in out Calculator) with
    Pre => (StackSize(C) >= 2 and C.Lock_State = False),
    Post => (StackSize(C) <= StackSize(C'Old));

    procedure Divide(C: in out Calculator) with
    Pre => (StackSize(C) >= 2 and C.Lock_State = False),
    Post => (StackSize(C) <= StackSize(C'Old));

    function StackSize(C: in Calculator) return Integer;

    private

       function StackSize(C: in Calculator) return Integer is 
       (Stack.Size(C.OperandStack));

end calculator;
