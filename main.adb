pragma SPARK_Mode (On);

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
   DB : VariableStore.Database;
   V1 : VariableStore.Variable := VariableStore.From_String("Var1");
   PIN1  : PIN.PIN;
   package Lines is new MyString(Max_MyString_Length => 2048);
   S  : Lines.MyString;
   Invalid_Message : String := "Invalid Input, Calculator Will Exit Now";
   Too_Many_Tokens: String := "Too many tokens";
   No_Tokens: String := "There is no token";
   Incorrect_Pin: String := "The pin entered is incorrect";
   Expect_Two_Tokens: String := "Expect 2 tokens but only get 1";
   Already_Unlocked: String := "Calculator already been unlocked";
   Already_Locked : String := "Calculator already been locked";
   
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
   
   loop
      if (Lock_State = Locked) then Put("locked>");
      else  put("unlocked>");
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
      
       -- Unlock Function Part
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
       
       -- Lock Function Part        
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
