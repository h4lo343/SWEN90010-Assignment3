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
   MaxSize: Integer := 512;
   package Stack is new SimpleStack(MaxSize, Integer, 0);
   OperandStack : Stack.SimpleStack;
   DB : VariableStore.Database;
   V1 : VariableStore.Variable := VariableStore.From_String("Var1");
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
   if (MyCommandLine.Argument_Count = 1 and 
   MyCommandLine.Argument(1)'Length = 4 and       
   (for all I in MyCommandLine.Argument(1)'Range => 
      MyCommandLine.Argument(1)(I) >= '0' and
      MyCommandLine.Argument(1)(I) <= '9')) then 
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
                
       begin
         
       if NumTokens = 2 then
          Parameter := Lines.Substring(S,T(2).Start,T(2).Start+T(2).Length-1);
       end if;
      
       -- Unlock Command Part
       if (Lines.Equal(Command, Command_Unlock)) then 
         if(NumTokens = 1) then        
           Put_Line(Expect_Two_Tokens);
           return;         
         elsif(Lock_State = Unlocked) then
           Put_Line(Already_Unlocked);
         else                
           if PIN."="(PIN1, PIN.From_String(Lines.To_String(Parameter))) then   
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
         
         elsif(Stack.Size(OperandStack) + 1 = MaxSize) then
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
           
           elsif(Stack.Size(OperandStack) + 1 = MaxSize) then
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
   --  Put(MyCommandLine.Command_Name); Put_Line(" is running!");
   --  Put("I was invoked with "); Put(MyCommandLine.Argument_Count); Put_Line(" arguments.");
   --  for Arg in 1..MyCommandLine.Argument_Count loop
   --     Put("Argument "); Put(Arg,0); Put(": """);
   --     Put(MyCommandLine.Argument(Arg)); Put_Line("""");
   --  end loop;
   --  
   --  
   --  
   --  VariableStore.Init(DB);
   --  Put_Line("Adding an entry to the database");
   --  VariableStore.Put(DB,V1,10);
   --  
   --  Put_Line("Reading the entry:");
   --  Put(VariableStore.Get(DB,V1));
   --  New_Line;
   --  
   --  Put_Line("Printing out the database: ");
   --  VariableStore.Print(DB);
   --  
   --  Put_Line("Removing the entry");
   --  VariableStore.Remove(DB,V1);
   --  If VariableStore.Has_Variable(DB,V1) then
   --     Put_Line("Entry still present! It is: ");
   --     Put(VariableStore.Get(DB,V1));
   --     New_Line;
   --  else
   --     Put_Line("Entry successfully removed");
   --  end if;
   --  
   --  Put_Line("Reading a line of input. Enter some text (at most 3 tokens): ");
   --  Lines.Get_Line(S);
   --  
   --  Put_Line("Splitting the text into at most 5 tokens");
   --  declare
   --     T : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
   --     NumTokens : Natural;
   --  begin
   --     MyStringTokeniser.Tokenise(Lines.To_String(S),T,NumTokens);
   --     Put("You entered "); Put(NumTokens); Put_Line(" tokens.");
   --     for I in 1..NumTokens loop
   --        declare
   --           TokStr : String := Lines.To_String(Lines.Substring(S,T(I).Start,T(I).Start+T(I).Length-1));
   --        begin
   --           Put("Token "); Put(I); Put(" is: """);
   --           Put(TokStr); Put_Line("""");
   --        end;
   --     end loop;
   --     if NumTokens > 3 then
   --        Put_Line("You entered too many tokens --- I said at most 3");
   --     end if;
   --  end;
   --  
   --  If PIN."="(PIN1,PIN2) then
   --     Put_Line("The two PINs are equal, as expected.");
   --  end if;
   --  
   --  declare
   --     Smallest_Integer : Integer := StringToInteger.From_String("-2147483648");
   --     R : Long_Long_Integer :=
   --       Long_Long_Integer(Smallest_Integer) * Long_Long_Integer(Smallest_Integer);
   --  begin
   --     Put_Line("This is -(2 ** 32) (where ** is exponentiation) :");
   --     Put(Smallest_Integer); New_Line;
   --  
   --     if R < Long_Long_Integer(Integer'First) or
   --        R > Long_Long_Integer(Integer'Last) then
   --        Put_Line("Overflow would occur when trying to compute the square of this number");
   --     end if;
   --  
   --  end;
   --  Put_Line("2 ** 32 is too big to fit into an Integer...");
   --  Put_Line("Hence when trying to parse it from a string, it is treated as 0:");
   --  Put(StringToInteger.From_String("2147483648")); New_Line;
   end loop;
      
end Main;
