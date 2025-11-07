import random

class ObfuscatedSmolGenerator:
    """
    Generates random, syntactically valid, and intentionally obfuscated
    'smol' language programs for testing LLM context rot.
    
    Generates 'main (bool) [...]' or 'main (int) [...]' programs.
    - All expressions are fully parenthesized to remove ambiguity.
    - Loops (doth) are guaranteed to terminate.
    - 'bool' programs have meaningful control flow (if/else return 'truth', final returns 'false', or vice versa).
    - 'int' programs have return values dependent on variables mutated in loops.
    """
    def __init__(self, max_depth=3, max_statements_per_block=3):
        self.max_depth = max_depth
        self.max_statements = max_statements_per_block
        
        # State tracking
        self.vars_in_scope = set()
        self.var_counter = 0
        self.return_type = 'bool'
        
        # For 'bool' programs:
        # We enforce a contrast strategy to make control flow matter.
        self.bool_return_logic = {}
        
        # For 'int' programs:
        # We track vars mutated in loops to make return values matter.
        self.vars_used_in_loops = set()

    def new_var_name(self):
        """Generates a new, unique variable name like v1, v2, etc."""
        self.var_counter += 1
        return f"v{self.var_counter}"

    def make_indent(self, depth):
        """Returns an indentation string for the given depth."""
        return "  " * depth

    def create_new_var(self, depth):
        """
        Creates a new variable, adds it to scope, and returns its name.
        FIX: All variables are treated as globally scoped for simplicity
        and to allow for more complex data flow.
        """
        var_name = self.new_var_name()
        # if depth <= 1: # Original flawed logic
        self.vars_in_scope.add(var_name) # All vars are global
        return var_name

    def generate_program(self):
        """Generates a full 'main (bool|int) [...]' program."""
        # Reset state
        self.vars_in_scope.clear()
        self.var_counter = 0
        self.vars_used_in_loops.clear()
        self.return_type = random.choice(['bool', 'int'])

        # Set up the boolean logic strategy
        if self.return_type == 'bool':
            if random.random() < 0.5:
                # 'if' branches return 'truth', final return is 'false'
                self.bool_return_logic = {'if_val': 'truth', 'final_val': 'false'}
            else:
                # 'if' branches return 'false', final return is 'truth'
                self.bool_return_logic = {'if_val': 'false', 'final_val': 'truth'}
        else:
            self.bool_return_logic = {} # Not used for int

        # Create a few initial variables with simple values
        statements = []
        # Ensure at least 2 vars exist so loops can always find a var to mutate
        for i in range(random.randint(2, 4)):
            var_name = self.create_new_var(depth=1)
            value = random.randint(1, 20)
            statements.append(f"{self.make_indent(1)}{value} -> {var_name}:")
        
        if self.return_type == 'bool':
            # --- LOGIC FOR BOOL ---
            # 1. Force at least one 'if' statement to make logic non-trivial
            statements.append(self.generate_if(1))
            
            # 2. Add a few other random statements (assign/loop)
            num_extra_statements = random.randint(0, self.max_statements - 1)
            for _ in range(num_extra_statements):
                statements.append(self.generate_statement(1, allow_loop=True)) 
        else:
            # --- LOGIC FOR INT ---
            # 1. Force at least one 'doth' loop to mutate variables
            statements.append(self.generate_loop(1))
            
            # 2. Add a few other random statements (assign/if)
            num_extra_statements = random.randint(0, self.max_statements - 1)
            for _ in range(num_extra_statements):
                statements.append(self.generate_statement(1, allow_loop=False)) # No nested loops
            
        # Add the required return statement
        statements.append(self.generate_return(1))
        
        # Assemble the program
        body = "\n".join(statements)
        return f"main ({self.return_type}) [\n{body}\n]"


    def generate_statement(self, depth, allow_loop=True):
        """Generates a random statement (assignment, if, or loop)."""
        indent_str = self.make_indent(depth)
        comment = f"{indent_str}@ context rot check"
        
        if depth >= self.max_depth:
            # Must be assignment if we are too deep
            choice = 'assign'
        else:
            choices = ['assign', 'if']
            if allow_loop:
                choices.append('loop')
            choice = random.choice(choices)

        if choice == 'assign':
            return comment + "\n" + self.generate_assignment(depth)

        if choice == 'if':
            return comment + "\n" + self.generate_if(depth)

        if choice == 'loop':
            # Note: Loops are only generated at depth 1, per generate_program logic
            return comment + "\n" + self.generate_loop(depth)

    def generate_assignment(self, depth):
        """Generates: expr -> var:"""
        indent_str = self.make_indent(depth)
        
        is_new_var = False
        
        if depth > 1 and self.vars_in_scope:
            # 1. Nested block (in 'if' or 'loop'): MUST reuse a var
            #    (This was the fix for the v5 error)
            var_name = random.choice(list(self.vars_in_scope))
        elif not self.vars_in_scope or (depth == 1 and random.random() < 0.3):
            # 2. Top level (or no vars exist): Create NEW var
            var_name = self.create_new_var(depth) # create_new_var adds to scope
            is_new_var = True
        else:
            # 3. Top level: Reuse existing var
            var_name = random.choice(list(self.vars_in_scope))

        # FIX: If we just created a new variable (e.g., v4),
        # we must not allow generate_expression to read it
        # on the right-hand side, e.g., "(v4 + 10) -> v4:".
        # Temporarily remove it from scope for expression generation.
        if is_new_var:
            self.vars_in_scope.remove(var_name)
            
        expr = self.generate_expression(depth + 1)
        
        # Add the new var back to the scope after the expression is generated
        if is_new_var:
            self.vars_in_scope.add(var_name)
            
        # The 'generate_expression' function already handles
        # parenthesizing complex expressions (e.g., "(v1 + v2)").
        return f"{indent_str}{expr} -> {var_name}:"

    def generate_expression(self, depth, force_complex=False):
        """
        Generates a numeric expression, fully parenthesized.
        """
        if (depth >= self.max_depth or random.random() < 0.3) and not force_complex:
            # Base case: literal or variable
            if random.random() < 0.5 or not self.vars_in_scope:
                return str(random.randint(1, 50))
            else:
                # self.vars_in_scope is now guaranteed to contain all vars
                return random.choice(list(self.vars_in_scope))
        
        # Recursive case: binary operation
        left = self.generate_expression(depth + 1)
        right = self.generate_expression(depth + 1)
        op = random.choice(['+', '-', '*'])
        
        return f"({left} {op} {right})"

    def generate_condition(self, depth):
        """Generates: { expr op expr }"""
        left = self.generate_expression(depth + 1)
        right = self.generate_expression(depth + 1)
        
        # GTE_INV (=>) is the most confusing, so let's use it often
        if random.random() < 0.5:
            op = "=>"
        else:
            op = random.choice(['==', '>'])
            
        return f"{{ {left} {op} {right} }}"

    def generate_if(self, depth):
        """Generates: if {cond} [ block ] (though_if {cond} [ block ])*"""
        indent_str = self.make_indent(depth)
        
        # If branch
        cond = self.generate_condition(depth + 1)
        block = self.generate_block(depth + 1)
        if_str = f"{indent_str}if {cond} {block}"
        
        # though_if branches
        num_though_ifs = random.randint(0, 1)
        for _ in range(num_though_ifs):
            cond = self.generate_condition(depth + 1)
            block = self.generate_block(depth + 1)
            if_str += f"\n{indent_str}though_if {cond} {block}"
            
        return if_str

    def generate_block(self, depth):
        """Generates a [ block ] of statements."""
        indent_str = self.make_indent(depth)
        num_statements = random.randint(1, self.max_statements)
        
        statements = []
        for _ in range(num_statements):
            # No loops inside 'if' blocks, too complex
            statements.append(self.generate_statement(depth, allow_loop=False)) 
            
        # Every block in 'bool' mode MUST end in a return
        if self.return_type == 'bool':
            statements.append(self.generate_return(depth))
            
        body = "\n".join(statements)
        return f"[\n{body}\n{indent_str}]"

    def generate_loop(self, depth):
        """
        Generates a doth [ ... ] while { 0 => counter } loop.
        Guarantees termination.
        """
        indent_str = self.make_indent(depth)
        
        # 1. Create a dedicated counter variable for this loop
        # This is safe; generate_loop is only called at depth 1
        counter_var = self.create_new_var(depth)
        init_val = random.randint(3, 5) # Loop 3-5 times
        init_assign = f"{indent_str}{init_val} -> {counter_var}:"

        # 2. Create the loop body
        body_indent_str = self.make_indent(depth + 1)
        
        # 2a. The decrement statement
        decrement_stmt = f"{body_indent_str}({counter_var} - 1) -> {counter_var}:"
        
        # 2b. A statement that mutates another variable
        # This is the "heavy use" we want to track
        
        # This check is safe, program guarantees >= 2 initial vars
        # and counter_var has just been added.
        possible_vars = list(self.vars_in_scope - {counter_var})
        if not possible_vars:
            # This should be impossible, but as a failsafe:
            # Create a new var to mutate.
            var_name = self.create_new_var(depth)
            possible_vars.append(var_name)

        var_to_mutate = random.choice(possible_vars)
        
        # Track this variable as "dirty"
        if depth == 1: # Only track top-level loop mutations
            self.vars_used_in_loops.add(var_to_mutate)
            
        # e.g., ((v1 * v_counter) + 5) -> v1:
        # We can now use counter_var in the expression, as it's in scope
        loop_expr = self.generate_expression(depth + 2)
        mutate_stmt = f"{body_indent_str}({loop_expr} + {var_to_mutate}) -> {var_to_mutate}:"
        
        # 2c. (Optional) nested 'if'
        extra_statements = [decrement_stmt, mutate_stmt]
        if random.random() > 0.5:
             # FIX: Temporarily remove the counter_var from the scope
             # so that the nested 'if' statement's assignments
             # (generated by generate_assignment) cannot randomly
             # pick it and break the loop's termination.
             self.vars_in_scope.remove(counter_var)
             extra_statements.append(self.generate_if(depth + 1))
             # Add it back after generation
             self.vars_in_scope.add(counter_var)

        random.shuffle(extra_statements)
        
        body_str = f"[\n" + "\n".join(extra_statements) + f"\n{indent_str}]"
        
        # 3. Create the condition
        # { 0 => counter_var } (means counter_var >= 0)
        condition_str = f"{{ 0 => {counter_var} }}"
        
        return f"{init_assign}\n{indent_str}doth {body_str} while {condition_str}"
        
    def generate_return(self, depth):
        """
        Generates a return statement based on program type.
        """
        indent_str = self.make_indent(depth)
        
        if self.return_type == 'bool':
            # Use the pre-defined logic
            if depth == 1:
                # This is the FINAL return statement
                val = self.bool_return_logic['final_val']
            else:
                # This is a return from an 'if' block
                val = self.bool_return_logic['if_val']
            return f"{indent_str}return {val}:"
        
        else: # 'int'
            # This logic is only called for the FINAL return (depth=1)
            # 'if' blocks don't return ints in this design
            
            # Force a complex expression using at least one "dirty" var
            if not self.vars_used_in_loops:
                # Failsafe: loop logic failed, just use any var
                if not self.vars_in_scope: self.create_new_var(1) # double failsafe
                dirty_var = random.choice(list(self.vars_in_scope))
            else:
                dirty_var = random.choice(list(self.vars_used_in_loops))

            # e.g., (v1 * (dirty_var + 10))
            other_expr = self.generate_expression(depth + 1)
            val = f"({dirty_var} + {other_expr})"
            
            return f"{indent_str}return {val}:"


# --- Example Usage ---
if __name__ == "__main__":
    print("--- Obfuscated Smol Language Generator ---")
    gen = ObfuscatedSmolGenerator(max_depth=3, max_statements_per_block=3)
    
    for _ in range(2):
        print("\n--- NEW PROGRAM (BOOL) ---")
        # Force bool for example, but need to call generate_program
        # which sets its own type. We can set it manually *after* init.
        gen.return_type = 'bool' 
        print(gen.generate_program())
        
    for _ in range(2):
        print("\n--- NEW PROGRAM (INT) ---")
        gen.return_type = 'int' # Force int for example
        print(gen.generate_program())