pragma SPARK_Mode (On);
with SimpleStack;
with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with Ada.Long_Long_Integer_Text_IO;

procedure Main is
 
   package Stack is new SimpleStack(512, Integer, 0);
   OperandStack : Stack.SimpleStack;
   DB : VariableStore.Database;
   PIN1  : PIN.PIN;
   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;
   
   Invalid_Message : String := "Invalid Input, Calculator Will Exit Now";
   Too_Many_Tokens: String := "Too Many Tokens";
   No_Tokens: String := "There Is No Token";
   Incorrect_Pin: String := "The Pin Entered Is Incorrect";
   Expect_Two_Tokens: String := "Expect 2 Tokens But Only Get 1";
   Expect_One_Token: String := "Only Expect 1 Command For The Command";
   Already_Unlocked: String := "Calculator Already Been Unlocked";
   Already_Locked : String := "Calculator Already Been Locked";
   No_Unlocked : String := "Have Not Be Unlocked Yet, Calculator Will Exit Now";
   Too_Many_Operand : String := "Too Many Operands In Stack, Pop Some First";
   No_Enough_Operand : String := "No Enough Operands For the Operation";
   No_Variable : String := "There Is No Such Variable In Database";
   Divide_By_Zero: String := "Can not divide by 0, Calculator Will Exit Now";
   
   type Lock_State_Type is (Locked, Unlocked);
   Lock_State : Lock_State_Type;
   
   Command_Add : Lines.MyString := Lines.From_String("+");
   Command_Minus : Lines.MyString := Lines.From_String("-");
   Command_Multiple : Lines.MyString := Lines.From_String("*");
   Command_Divide : Lines.MyString := Lines.From_String("/");
   Command_Push : Lines.MyString := Lines.From_String("push");
   Command_Pop : Lines.MyString := Lines.From_String("pop");
   Command_Load : Lines.MyString := Lines.From_String("load");
   Command_Store : Lines.MyString := Lines.From_String("store");
   Command_Remove : Lines.MyString := Lines.From_String("remove");
   Command_List : Lines.MyString := Lines.From_String("list");
   Command_Unlock : Lines.MyString := Lines.From_String("unlock");
   Command_Lock : Lines.MyString := Lines.From_String("lock");
   
begin
   if (MyCommandLine.Argument_Count = 1) then
   if (MyCommandLine.Argument(1)'Length = 4) and       
   (for all I in MyCommandLine.Argument(1)'Range => 
      MyCommandLine.Argument(1)(I) >= '0' and
      MyCommandLine.Argument(1)(I) <= '9') then 
      PIN1 := PIN.From_String(MyCommandLine.Argument(1));
      Lock_State := Locked;
   else
      Put_Line(Invalid_Message);
      return;
      
   end if;
   
   Stack.Init(OperandStack);
   VariableStore.Init(DB);
   
   loop
      if (Lock_State = Locked) then Put("locked>");
      else put("unlocked>");
      end if;  
      
      Lines.Get_Line(S);
      if Lines.length(S) > 2048 then
         Put_Line(Invalid_Message);
         return;
      end if;   
      
    declare
          T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
          NumTokens : Natural;
       begin 
          MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
          if NumTokens > 2 then
            Put_Line(Too_Many_Tokens);
            return;
          elsif NumTokens < 1 then
            Put_Line(No_Tokens);   
            return; 
          end if;
      
       declare 
       Command : Lines.MyString := Lines.Substring(S,T(1).Start,T(1).Start+T(1).Length-1);
       Parameter : Lines.MyString := Lines.From_String("");
       Patameter_String : String := Lines.To_String(Parameter);  
               
       begin
         
       if NumTokens = 2 then
          Parameter := Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1);
       end if;
      
       -- Unlock Command Part
       if (Lines.Equal(Command, Command_Unlock)) then 
         if(NumTokens = 1) then        
           Put_Line(Expect_Two_Tokens);
           return;         
         
         elsif(Patameter_String' Length /= 4 or (for all I in Patameter_String' Range => 
          Patameter_String(I) <= '0' or Patameter_String(I) >= '9'  )) then
          Put_Line(Invalid_Message);            
          return;          
                    
         elsif(Lock_State = Unlocked) then
           Put_Line(Already_Unlocked);
         else                
           if PIN."="(PIN1, PIN.From_String(Patameter_String)) then   
             Lock_State := Unlocked; 
           else        
           Put_Line(Incorrect_Pin);
           end if;                 
         end if;  
       
       -- Lock Command Part        
       elsif (Lines.Equal(Command, Command_Lock)) then 
         if(NumTokens = 1) then        
           Put_Line(Expect_Two_Tokens);
           return;         
         
         elsif(Lock_State = Locked) then
           Put_Line(Already_Locked);
         
         elsif(Patameter_String' Length /= 4 or (for all I in Patameter_String' Range => 
          Patameter_String(I) <= '0' or Patameter_String(I) >= '9'  )) then
          Put_Line(Invalid_Message);            
          return; 
                  
         else                
           if PIN."="(PIN1, PIN.From_String(Lines.To_String(Parameter))) then   
             Lock_State := Locked;
           else        
           Put_Line(Incorrect_Pin);       
           end if;                 
         end if;         
       
       -- Push Command Part        
       elsif (Lines.Equal(Command, Command_Push)) then
         if(Lock_State = Locked) then
           Put_Line(No_Unlocked);      
           return;        
         
         elsif(NumTokens = 1) then
           Put_Line(Expect_Two_Tokens);
           return;   
         
         elsif(Stack.Size(OperandStack) = 512) then
           Put_Line(Too_Many_Operand);       
           return;       
         
         else 
         Stack.Push(OperandStack, StringToInteger.From_String(Lines.To_String(Parameter)));
         end if;  
       
       -- Minus Command Part        
       elsif (Lines.Equal(Command, Command_Minus)) then
         declare
           IntTemp1 : Integer;     
           IntTemp2 : Integer;
         begin      
           if(Lock_State = Locked) then
             Put_Line(No_Unlocked);      
             return;        
             
           elsif(NumTokens = 2) then
             Put_Line(Expect_One_Token);
             return; 
       
           elsif(Stack.Size(OperandStack) < 2) then     
             Put_Line(No_Enough_Operand);
             return; 
         
           else
             Stack.Pop(OperandStack, IntTemp1);     
             Stack.Pop(OperandStack, IntTemp2);  
                        
             if (IntTemp1 < Integer'First + IntTemp2 or IntTemp1 > Integer'Last + IntTemp2 ) then             
                Put_Line(Invalid_Message);             
                return;       
             end if;           
                        
             Stack.Push(OperandStack, IntTemp1 - IntTemp2);
           end if;         
         end;        
               
       -- Add Command Part
       elsif (Lines.Equal(Command, Command_Add)) then
         declare
           IntTemp1 : Integer;     
           IntTemp2 : Integer;
         begin      
           if(Lock_State = Locked) then
             Put_Line(No_Unlocked);      
             return;        
         
           elsif(NumTokens = 2) then
             Put_Line(Expect_One_Token);
             return; 
       
           elsif(Stack.Size(OperandStack) < 2) then     
             Put_Line(No_Enough_Operand);
             return; 
         
           else
             Stack.Pop(OperandStack, IntTemp1);     
             Stack.Pop(OperandStack, IntTemp2);          
             Stack.Push(OperandStack, IntTemp1 + IntTemp2);
           end if;         
         end;                     
       
       -- Multiple Command Part        
       elsif (Lines.Equal(Command, Command_Multiple)) then         
         declare         
           IntTemp1 : Integer;     
           IntTemp2 : Integer;      
         begin       
           if(Lock_State = Locked) then
             Put_Line(No_Unlocked);      
             return;        
         
           elsif(NumTokens = 2) then
             Put_Line(Expect_One_Token);
             return; 
       
           elsif(Stack.Size(OperandStack) < 2) then     
             Put_Line(No_Enough_Operand);
             return;
                  
           else
             Stack.Pop(OperandStack, IntTemp1);     
             Stack.Pop(OperandStack, IntTemp2);          
             Stack.Push(OperandStack, IntTemp1 * IntTemp2);          
                     
           end if;             
         end; 
                   
               
       -- Divide Command Part        
       elsif(Lines.Equal(Command, Command_Divide)) then        
         declare         
           IntTemp1 : Integer;     
           IntTemp2 : Integer;      
         begin       
           if(Lock_State = Locked) then
             Put_Line(No_Unlocked);      
             return;        
         
           elsif(NumTokens = 2) then
             Put_Line(Expect_One_Token);
             return; 
       
           elsif(Stack.Size(OperandStack) < 2) then     
             Put_Line(No_Enough_Operand);
             return;      
                     
           else
             Stack.Pop(OperandStack, IntTemp1);     
             Stack.Pop(OperandStack, IntTemp2);          
             
             if(IntTemp2 = 0) then Put_Line(Divide_By_Zero); return; end if;        
                     
             Stack.Push(OperandStack, IntTemp1 / IntTemp2);          
                     
           end if;             
         end;         
               
       -- Pop Command Part   
       elsif (Lines.Equal(Command, Command_Pop)) then
       declare 
         temp : Integer;        
       begin         
       if(Lock_State = Locked) then
           Put_Line(No_Unlocked);      
             return; 
                  
       elsif(NumTokens = 2) then
             Put_Line(Expect_One_Token);
             return;
       
       elsif(Stack.Size(OperandStack) < 1) then     
             Put_Line(No_Enough_Operand);
             return;           
       
       else 
                  
       Stack.Pop(OperandStack, temp);   
                        
       end if;        
       end;        
               
       -- Store Command Part    
       elsif (Lines.Equal(Command, Command_Store)) then     
         declare     
           variableName: String := Lines.To_String(Parameter);     
           variable: VariableStore.Variable := VariableStore.From_String(variableName);
           IntTemp: Integer;     
         
         begin         
                  
           if(Lock_State = Locked) then
           Put_Line(No_Unlocked);      
           return; 
            
           elsif(NumTokens = 1) then
             Put_Line(Expect_Two_Tokens);
             return;     
            
           elsif(Stack.Size(OperandStack) < 1) then
             Put_Line(No_Enough_Operand);
             return;        
       
           elsif(Lines.Length(Parameter) > 1024) then      
             Put_Line(Invalid_Message);           
             return;    
           else 
             if(VariableStore.Has_Variable(DB, variable)) then 
               VariableStore.Remove(DB, variable);
                                 
             end if;            
             Stack.Pop(OperandStack, IntTemp);  
             VariableStore.Put(DB,variable,IntTemp);       
           end if;    
         end;   
      
       
               
       -- Remove Command Part        
       elsif (Lines.Equal(Command, Command_Remove)) then        
         declare 
           variableName: String := Lines.To_String(Parameter);     
           variable: VariableStore.Variable := VariableStore.From_String(variableName);       
         
         begin 
           if(Lock_State = Locked) then
             Put_Line(No_Unlocked);      
             return; 
            
           elsif(NumTokens = 1) then
             Put_Line(Expect_Two_Tokens);
             return;              
          
           elsif(VariableStore.Has_Variable(DB, variable) = false) then        
             Put_Line(No_Variable);
             return;       
             
           else 
             VariableStore.Remove(DB, variable);        
                   
           end if;    
               
         end;       
               
       -- Load Command Part     
       elsif (Lines.Equal(Command, Command_Load)) then 
         declare  
           variableName: String := Lines.To_String(Parameter);     
           variable: VariableStore.Variable := VariableStore.From_String(variableName);
           IntTemp: Integer;                                   
                  
         begin           
           if(Lock_State = Locked) then
             Put_Line(No_Unlocked);      
             return; 
            
           elsif(NumTokens = 1) then
             Put_Line(Expect_Two_Tokens);
             return;              
          
           elsif(VariableStore.Has_Variable(DB, variable) = false) then        
             Put_Line(No_Variable);
             return;        
           
           elsif(Stack.Size(OperandStack) + 1 = 512) then
             Put_Line(Too_Many_Operand);       
             return;          
            
           else 
             IntTemp := VariableStore.Get(DB, variable);
             Stack.Push(OperandStack, IntTemp);              
                  
           end if;          
         end;
         
       -- List Command Part      
       elsif (Lines.Equal(Command, Command_List)) then         
          if(Lock_State = Locked) then
             Put_Line(No_Unlocked);      
             return;      
          
          elsif(NumTokens = 2) then
             Put_Line(Expect_One_Token);
             return;         
               
          else      
          VariableStore.Print(DB);
  
          end if;     
       else
       Put_Line(Invalid_Message);
       return;          
       
       end if;  
       end;       
    end;     

   end loop;
   end if;   
end Main;
