with VariableStore;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Calculator is


   procedure Init(C : out Calculator; P : in PIN.PIN) is
   begin
      VariableStore.Init(C.DB);
      C.MasterPin := P;
      Stack.Init(C.OperandStack);
      C.Lock_State := True;
   end Init;


   procedure Unlock(C : in out Calculator; P : in PIN.PIN) is
   begin
      if PIN."="(C.MasterPin, P) then
         C.Lock_State := False;
      end if;
   end Unlock;

   procedure Lock(C : in out Calculator; P : in PIN.PIN) is
   begin
      C.MasterPin := P;
      C.Lock_State := True;
   end Lock;

   procedure Push(C : in out Calculator; N: in Integer) is
   begin
      Stack.Push(C.OperandStack, N);
   end Push;

   procedure Store(C : in out Calculator; V: VariableStore.Variable) is
   I: Integer;
   begin
      Stack.Pop(C.OperandStack, I); 
      VariableStore.Put(C.DB, V, I);
   end Store;

   procedure Remove(C : in out Calculator; V: VariableStore.Variable) is
   begin
      VariableStore.Remove(C.DB, V);
   end Remove;

   procedure Pop(C : in out Calculator) is
   I: Integer;
   begin
      Stack.Pop(C.OperandStack, I);
      Put_Line("Pop integer: " & I'Image);
   end Pop;

   procedure Load(C : in out Calculator; V: VariableStore.Variable) is
   I: Integer;
   begin
      I := VariableStore.Get(C.DB, V);
      Stack.Push(C.OperandStack, I); 
   end Load;

   procedure List(C : in Calculator) is
   begin
      VariableStore.Print(C.DB);
   end List;


end calculator;
