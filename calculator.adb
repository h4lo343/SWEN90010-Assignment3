
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
end calculator;
