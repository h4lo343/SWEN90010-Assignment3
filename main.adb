pragma SPARK_Mode (On);
with SimpleStack;
with StringToInteger;
with VariableStore;
with MyCommandLine;
with MyString;
with MyStringTokeniser;
with PIN;
with Calculator;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers;
use Ada.Containers;

with Ada.Long_Long_Integer_Text_IO;

procedure Main is
   C: Calculator.Calculator;
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
   Overflow_Occur: String := "Overflow occured in the operation, Calculator Will Exit Now";
   DB_FULL: String := "DB is Full";
   

   
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
         Calculator.Init(C, PIN.From_String(MyCommandLine.Argument(1)));
   else
      Put_Line(Invalid_Message);
      return;
      
   end if;
   
--     Stack.Init(OperandStack);
--     VariableStore.Init(DB);
   
   loop
        if (C.Lock_State) then Put("locked>");
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
        declare 
           Parameter_String : String := Lines.To_String(Parameter);
        begin 
         if(NumTokens = 1) then        
           Put_Line(Expect_Two_Tokens);
           return;         
         
         elsif(Parameter_String'Length /= 4 or (for all I in Parameter_String'Range => 
          Parameter_String(I) < '0' or Parameter_String(I) > '9'  )) then
          Put_Line(Invalid_Message);            
          return;          
                    
         elsif(C.Lock_State = False) then
           Put_Line(Already_Unlocked);           
                        
         elsif(Parameter_String'Length = 4 and (for all I in Parameter_String'Range => 
          Parameter_String(I) >= '0' and Parameter_String(I) <= '9'  )) then 
                  
           Calculator.Unlock(C, PIN.From_String(Parameter_String));
         end if;  
       end;


       -- Lock Command Part        
       elsif (Lines.Equal(Command, Command_Lock)) then 
        declare 
           Parameter_String : String := Lines.To_String(Parameter);
        begin 
         if(NumTokens = 1) then        
           Put_Line(Expect_Two_Tokens);
           return;         
         
         elsif(C.Lock_State) then
           Put_Line(Already_Locked);
                      
                        
         elsif(Parameter_String'Length /= 4 or (for all I in Parameter_String'Range => 
          Parameter_String(I) < '0' or Parameter_String(I) > '9'  )) then
          Put_Line(Invalid_Message);            
          return; 

         elsif(Parameter_String'Length = 4 and (for all I in Parameter_String'Range => 
         Parameter_String(I) >= '0' and Parameter_String(I) <= '9'  )) then                 
             Calculator.Lock(C, PIN.From_String(Parameter_String));
         end if;   
        end;

        -- Push Command Part        
        elsif (Lines.Equal(Command, Command_Push)) then
                 
         if(C.Lock_State) then
           Put_Line(No_Unlocked);      
           return;                   
                     
         elsif(NumTokens = 1) then
           Put_Line(Expect_Two_Tokens);
           return;   
         
         elsif(Calculator.StackSize(C) = 512) then
           Put_Line(Too_Many_Operand);       
           return;       
                       
         else 
         Calculator.Push(C, StringToInteger.From_String(Lines.To_String(Parameter)));
--         Stack.Push(OperandStack, StringToInteger.From_String(Lines.To_String(Parameter)));
         end if;                
--       end if;  

        -- Store Command Part    
       elsif (Lines.Equal(Command, Command_Store)) then     
         declare     
           variableName: String := Lines.To_String(Parameter);     
           variable: VariableStore.Variable;
         begin         
           if(variableName'Length > 1024) then
             Put_Line(Invalid_Message);           
             return; 
           else 
             variable := VariableStore.From_String(variableName);    
           end if;       
           if(C.Lock_State) then
           Put_Line(No_Unlocked);      
           return; 
                      
                        
           elsif(NumTokens = 1) then
             Put_Line(Expect_Two_Tokens);
             return;     
            
           elsif(Calculator.StackSize(C) < 1) then
             Put_Line(No_Enough_Operand);
               
           else 
             if(VariableStore.Has_Variable(C.DB, variable)) then 
               VariableStore.Remove(C.DB, variable);
                                 
             end if;    
             if(VariableStore.Length(C.DB) >= 1000 ) then
               Put_Line(DB_FULL);
               return;
             else
             Calculator.Store(C, variable);
             end if;      
           end if;    
         end;

        -- Remove Command Part        
       elsif (Lines.Equal(Command, Command_Remove)) then        
         declare 
           variableName: String := Lines.To_String(Parameter);     
           variable: VariableStore.Variable;       
         
         begin 
           if(variableName'Length > 1024) then
             Put_Line(Invalid_Message);
             return;
           else 
             variable := VariableStore.From_String(variableName); 
           end if;  
           if(C.Lock_State) then
             Put_Line(No_Unlocked);      
             return;          
                        
           elsif(NumTokens = 1) then
             Put_Line(Expect_Two_Tokens);
             return;              
          
           elsif(VariableStore.Has_Variable(C.DB, variable) = False) then        
             Put_Line(No_Variable);
             return;       
             
           else       
              Calculator.Remove(C, variable);
           end if;    
               
         end; 

       -- Pop Command Part   
       elsif (Lines.Equal(Command, Command_Pop)) then       
       begin         
       if(C.Lock_State) then
           Put_Line(No_Unlocked);      
             return;                
                        
       elsif(NumTokens = 2) then
             Put_Line(Expect_One_Token);
             return;
       
       elsif(Calculator.StackSize(C) < 1) then     
             Put_Line(No_Enough_Operand);
       
       else 
         Calculator.Pop(C);
--       Stack.Pop(OperandStack, temp);   
--       Put_Line("pop integer: " & temp'Image);                 
       end if;        
       end;

       -- Load Command Part     
       elsif (Lines.Equal(Command, Command_Load)) then 
         declare  
           variableName: String := Lines.To_String(Parameter);     
           variable: VariableStore.Variable;                                 
                  
         begin    
           if(variableName'Length > 1024) then
             Put_Line(Invalid_Message);
             return;
           else 
             variable := VariableStore.From_String(variableName); 
           end if;         
           if(C.Lock_State) then
             Put_Line(No_Unlocked);      
             return;           
                        
           elsif(NumTokens = 1) then
             Put_Line(Expect_Two_Tokens);
             return;              
          
           elsif(VariableStore.Has_Variable(C.DB, variable) = False) then        
             Put_Line(No_Variable);
             return;        
           
           elsif(Calculator.StackSize(C) = 512) then
             Put_Line(Too_Many_Operand);       
             return;          
            
           else 
             Calculator.Load(C, variable);              
                  
           end if;          
         end;

       -- List Command Part      
       elsif (Lines.Equal(Command, Command_List)) then         
          if(C.Lock_State) then
             Put_Line(No_Unlocked);      
             return;      
          
          elsif(NumTokens = 2) then
             Put_Line(Expect_One_Token);
             return;         
               
          else      
--          VariableStore.Print(C.DB);
          Calculator.List(C);
  
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
