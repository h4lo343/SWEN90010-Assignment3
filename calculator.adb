with VariableStore;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Calculator with SPARK_Mode is

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
      else
         Put_Line(Incorrect_Pin);
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

   procedure Add(C: in out Calculator) is
   I1: Integer;
   I2: Integer;
   begin
      Stack.Pop(C.OperandStack, I1);
      Stack.Pop(C.OperandStack, I2);
      if(if I2 < 0 then I1 >= Integer'First - I2
               else I1 <= Integer'Last - I2
             ) then 
             Stack.Push(C.OperandStack, I1 + I2);
      else 
         Put_Line(Overflow_Occur);
         Stack.Push(C.OperandStack, I2);
         Stack.Push(C.OperandStack, I1);
      end if;
   end Add;

   procedure Minus(C: in out Calculator) is
   I1: Integer;
   I2: Integer;
   begin
      Stack.Pop(C.OperandStack, I1);
      Stack.Pop(C.OperandStack, I2);
      if(if I2 < 0 then I1 <= Integer'Last + I2
                  else I1 >= Integer'First + I2
             ) then 
             Stack.Push(C.OperandStack, I1 - I2);
      else 
         Put_Line(Overflow_Occur);
         Stack.Push(C.OperandStack, I2);
         Stack.Push(C.OperandStack, I1);
      end if;
   end Minus;

   procedure Multiply(C: in out Calculator) is
   I1: Integer;
   I2: Integer;
   result : Integer := 0;  
   count : Integer := 0; 
   begin
      Stack.Pop(C.OperandStack, I1);
      Stack.Pop(C.OperandStack, I2);
      
      if(I2 > 0 and I1 > 0) then 
              while count < I1 loop
              if(result > Integer'Last - I2) then
                Put_Line(Overflow_Occur); 
                  Stack.Push(C.OperandStack, I2);
                  Stack.Push(C.OperandStack, I1);
                return;
              else   
                result := result + I2;
                count := count + 1;
              end if;               
              end loop;   
             

       elsif(I1 > 0 and I2 < 0) then 
        while count < I1 loop
        if(result < Integer'First - I2) then
          Put_Line(Overflow_Occur); 
            Stack.Push(C.OperandStack, I2);
            Stack.Push(C.OperandStack, I1);
          return;
        else   
          result := result + I2;
          count := count + 1;
        end if;               
        end loop;  

       elsif(I1 < 0 and I2 > 0) then 
        while count < I2 loop
        if(result < Integer'First - I1) then
          Put_Line(Overflow_Occur); 
          Stack.Push(C.OperandStack, I2);
          Stack.Push(C.OperandStack, I1);
          return;
        else   
          result := result + I1;
          count := count + 1;
        end if;               
        end loop;

       elsif(I1 < 0 and I2 < 0) then 
          if(I1 = Integer'First or I2 = Integer'First) then
            Put_Line(Overflow_Occur); 
            Stack.Push(C.OperandStack, I2);
            Stack.Push(C.OperandStack, I1);
            return;
          else   
          while count < (-I2) loop
          if(result > Integer'Last + I1) then
            Put_Line(Overflow_Occur); 
            Stack.Push(C.OperandStack, I2);
            Stack.Push(C.OperandStack, I1);
            return;
          else   
            result := result - I1;
            count := count + 1;
          end if;               
          end loop; 
          end if;
        
       end if;    
       Stack.Push(C.OperandStack, result);

   end Multiply;

   procedure Divide(C: in out Calculator) is
   I1: Integer;
   I2: Integer;
   Result : Integer := 0;
   Bound : Integer;
   begin
   Stack.Pop(C.OperandStack, I1);     
   Stack.Pop(C.OperandStack, I2);          

   if(I2 = 0) then Put_Line(Divide_By_Zero); return; end if;
   if (I1 = 0) then Stack.Push(C.OperandStack, 0); end if;
   if (I1 = Integer'First and I2 = -1) then
      Put_Line(Overflow_Occur);
      Stack.Push(C.OperandStack, I2);
      Stack.Push(C.OperandStack, I1); 
      return;
   end if;

   if (I1 > 0 and I2 > 0) then   
      Bound := I1;
      while I1 >= I2 and Result < Bound loop
         I1 := I1 - I2;
         Result := Result + 1;
      end loop;        
   end if;

   if (I1 > 0 and I2 < 0) then   
      Bound := I1;
      while -I1 <= I2 and Result > -Bound loop
         I1 := I1 + I2;
         Result := Result - 1;
      end loop;        
   end if;

   if (I1 < 0 and I2 > 0) then   
      Bound := I1;
      while I1 <= -I2 and Result > Bound loop
         I1 := I1 + I2;
         Result := Result - 1;
      end loop;        
   end if;

   if (I1 < 0 and I2 < 0) then   
      Bound := I1;
      while I1 <= I2 and Result > Bound loop
         I1 := I1 - I2;
         Result := Result - 1;
      end loop;  
      if(Result = Integer'First) then
         Put_Line(Overflow_Occur); 
         Stack.Push(C.OperandStack, I2);
         Stack.Push(C.OperandStack, I1);
         return;   
      end if;  
      Result := -Result;   
   end if;          
   Stack.Push(C.OperandStack, Result);

   end Divide;


end calculator;
