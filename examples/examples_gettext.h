/* Automatically generated file using generate_code.sh from the .wxm examples           */
/* This file is part of wxMaxima, but read only by gettext allowing to translate the    */
/* example files.                                                                       */

/* Copyright (C) 2019 wxMaxima Team (https://wxMaxima-developers.github.io/wxmaxima/)   */

/* This program is free software; you can redistribute it and/or modify                 */
/* it under the terms of the GNU General Public License as published by                 */
/* the Free Software Foundation; either version 2 of the License, or                    */
/* (at your option) any later version.                                                  */

/* This program is distributed in the hope that it will be useful,                      */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of                       */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                        */
/* GNU General Public License for more details.                                         */

/* You should have received a copy of the GNU General Public License                    */
/* along with this program; if not, write to the Free Software                          */
/* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA            */

#include <wx/wx.h>
#include <wx/string.h

wxString examples = 
wxT("/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/\n") 
wxT("/* [ Created with wxMaxima version 19.12.4-DevelopmentSnapshot ] */\n") 
wxT("/* [wxMaxima: title   start ]\n")  + _(
wxT("Solving equations\n") 
) + wxT("   [wxMaxima: title   end   ] */\n")  
wxT("\n") 
wxT("\n") 
wxT("/* [wxMaxima: comment start ]\n")  + _(
wxT("Mathematics can be seen as a language consisting only of a rather small set of words and only a handful of grammatical rules. Both sets are carefully chosen, though, that by simply reformulating the question in many cases the answer can be found.\n") 
) + wxT("   [wxMaxima: comment end   ] */\n")  
wxT("\n") 
wxT("\n") 
wxT("/* [wxMaxima: comment start ]\n")  + _(
wxT("If you are studying mathematics the good news is that solving equations is an art a computer can help with in many special cases but that often relies on human creativity for finding algorithms and ways.\n") 
) + wxT("   [wxMaxima: comment end   ] */\n")  
wxT("\n") 
wxT("\n") 
wxT("/* [wxMaxima: section start ]\n")  + _(
wxT("Simple equations\n") 
) + wxT("   [wxMaxima: section end   ] */\n")  
wxT("\n") 
wxT("\n") 
wxT("/* [wxMaxima: comment start ]\n")  + _(
wxT("For many problems Maxima's solve() program instantly finds a list of solutions:\n") 
) + wxT("   [wxMaxima: comment end   ] */\n")  
wxT("\n") 
wxT("\n") 
wxT("/* [wxMaxima: input   start ] */\n") 
wxT("pyth:a^2+b^2=c^2;\n") 
wxT("/* [wxMaxima: input   end   ] */\n") 
wxT("\n") 
wxT("\n") 
wxT("/* [wxMaxima: input   start ] */\n") 
wxT("sol1:solve(pyth,a);\n") 
wxT("/* [wxMaxima: input   end   ] */\n") 
wxT("\n") 
wxT("\n") 
wxT("/* [wxMaxima: comment start ]\n")  + _(
wxT("Both solutions solve() offers are valied. We can pick any of them manually, for example:\n") 
) + wxT("   [wxMaxima: comment end   ] */\n")  
wxT("\n") 
wxT("\n") 
wxT("/* [wxMaxima: input   start ] */\n") 
wxT("sol2:sol1[2];\n") 
wxT("/* [wxMaxima: input   end   ] */\n") 
wxT("\n") 
wxT("\n") 
wxT("/* [wxMaxima: comment start ]\n")  + _(
wxT("By having given this equation a name ("sol2") we gained a way to re-use this equation later:\n") 
) + wxT("   [wxMaxima: comment end   ] */\n")  
wxT("\n") 
wxT("\n") 
wxT("/* [wxMaxima: input   start ] */\n") 
wxT("solve(sol2,b);\n") 
wxT("/* [wxMaxima: input   end   ] */\n") 
wxT("/* [wxMaxima: question  start ] */\n") 
wxT("Question #1\n") 
wxT("/* [wxMaxima: question  end   ] */\n") 
wxT("/* [wxMaxima: answer  start ] */\n") 
wxT("\n") 
wxT("/* [wxMaxima: answer  end   ] */\n") 
wxT("/* [wxMaxima: question  start ] */\n") 
wxT("<mth><st>Is </st><mi>a</mi><st> positive, negative or zero?</st></mth>\n") 
wxT("/* [wxMaxima: question  end   ] */\n") 
wxT("/* [wxMaxima: answer  start ] */\n") 
wxT("p;\n") 
wxT("/* [wxMaxima: answer  end   ] */\n") 
wxT("\n") 
wxT("\n") 
wxT("/* [wxMaxima: comment start ]\n")  + _(
wxT("This time the type of solutions we can get depends on the value of a which is why maxima asked about it.\n") 
) + wxT("   [wxMaxima: comment end   ] */\n")  
wxT("\n") 
wxT("\n") 
wxT("/* [wxMaxima: comment start ]\n")  + _(
wxT("Solve also accepts lists as its arguments:\n") 
) + wxT("   [wxMaxima: comment end   ] */\n")  
wxT("\n") 
wxT("\n") 
wxT("/* [wxMaxima: input   start ] */\n") 
wxT("eq2:a+2*b=10;\n") 
wxT("/* [wxMaxima: input   end   ] */\n") 
wxT("\n") 
wxT("\n") 
wxT("\n") 
wxT("/* Old versions of Maxima abort on loading files that end in a comment. */\n") 
wxT(""Created with wxMaxima 19.12.4-DevelopmentSnapshot"$\n") 
" ";
