# I will attempt to build an expense calculator with minimal help
# First we will start by building the foundation
name = input ("What is Your Name?:")
monthly_income = float(input(f"Hey {name}, what is your monthly income?:")) # Float function transforms user input into a decimal number so we can use it later.

#Next we will allow the user to enter their expenses
expenses = {}
while True:
    category = input ("Enter the Expense Category (If you are done, enter 'done' to move on): ")
    if category == "done": break
    user_input = (input(f"How Much was Spent on {category}? "))
    if user_input == "done": break   # If user enters 'done', the loop will stop then the program will move to the next section.
    try: # Tells python to attempt the following lines but if they cause an error don't crash
        price = float(user_input) # 'float' function tries to turn the input into a number. If the user types a word, it will fail and ask to try again
        expenses [category] = price # If the conversions worked, this will add the new number into the list
    except ValueError:    # Stops the software from crashing and tries to find another way as it catches an error in the values.
        print ("Invalid input. Please enter a number.")



# Next we will make functions to calculate the expenses
total_spent = sum (expenses.values())
remaining_balance = monthly_income - total_spent
amount_exceeded = total_spent - monthly_income

# Displaying the Outputs
print ("--- Summary Of Expenses ---")
print (f"monthly income: £{monthly_income}")
for category, price in expenses.items():
    print (f"{category}: £{price}")

# Now we need to create print statements that return the details of the expenses
if remaining_balance > 0:
    print (f"You have money remaining from your budget: £{remaining_balance}")
elif remaining_balance == 0:
    print ("You have used up all of your budget")
else:
    print (f"You have exceeded your budget by: £{amount_exceeded}")
