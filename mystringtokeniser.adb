
package body MyStringTokeniser with SPARK_Mode is



   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) is
      Index : Positive;
      Extent : TokenExtent;
      OutIndex : Integer := Tokens'First;
   begin
      Count := 0;
      if (S'First > S'Last) then
         return;
      end if;
      Index := S'First;
      while OutIndex <= Tokens'Last and Index <= S'Last and Count < Tokens'Length loop

         -- This loop invariant is necessary because it makes sure all the tokens
         -- generated and stored in the 'Tokens' arrary during this procedure meet
         -- the following requirements:
         -- 1. Tokens(J).Start >= S'First:
         -- the starting index of token must equal or
         -- larger than then starting index of string 'S', which means, all the tokens
         -- should come from string 'S' and they can not access memory outside of sting 'S'
         -- 2. Tokens(J).Length > 0:
         -- all the tokens stored in 'Tokens' arrary should be
         -- non-empty string, which means, they are valid.
         -- 3. Tokens(J).Length-1 <= S'Last - Tokens(J).Start):
         -- the length of all tokens stored in 'Tokens'
         -- arrary should be equal or less than the length of the substring of 'S'
         -- (start from the starting index of the token, to the end of the string 'S')
         -- which means all the tokens are substring from 'S'
         -- and it is impossible for tokens to get memory outside of the string 'S'
         pragma Loop_Invariant
           (for all J in Tokens'First..OutIndex-1 =>
              (Tokens(J).Start >= S'First and
                   Tokens(J).Length > 0) and then
            Tokens(J).Length-1 <= S'Last - Tokens(J).Start);

         -- This loop invariant makes sure that each new token is placed in a right
         -- place in 'Tokens' array and the value of 'OutIndex' and 'Count' are always consistent
         -- by making sure that the 'OutIndex'(index for latest stored token) is equal to
         -- the sum of 'Tokens'First'(first index of token) and 'Count'
         -- (number of tokens currently generated).
         pragma Loop_Invariant (OutIndex = Tokens'First + Count);

         -- look for start of next token
         while (Index >= S'First and Index < S'Last) and then Is_Whitespace(S(Index)) loop
            Index := Index + 1;
         end loop;
         if (Index >= S'First and Index <= S'Last) and then not Is_Whitespace(S(Index)) then
            -- found a token
            Extent.Start := Index;
            Extent.Length := 0;

            -- look for end of this token
            while Positive'Last - Extent.Length >= Index and then (Index+Extent.Length >= S'First and Index+Extent.Length <= S'Last) and then not Is_Whitespace(S(Index+Extent.Length)) loop
               Extent.Length := Extent.Length + 1;
            end loop;

            Tokens(OutIndex) := Extent;
            Count := Count + 1;

            -- check for last possible token, avoids overflow when incrementing OutIndex
            if (OutIndex = Tokens'Last) then
               return;
            else
               OutIndex := OutIndex + 1;
            end if;

            -- check for end of string, avoids overflow when incrementing Index
            if S'Last - Extent.Length < Index then
               return;
            else
               Index := Index + Extent.Length;
            end if;
         end if;
      end loop;
   end Tokenise;

end MyStringTokeniser;
