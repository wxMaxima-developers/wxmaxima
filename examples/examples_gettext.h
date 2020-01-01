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
L"/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/\n" 
L"/* [ Created with wxMaxima version 19.12.4-DevelopmentSnapshot ] */\n" 
L"/* [wxMaxima: title   start ]\n"  + _(
L"Solving equations\n" 
) + L"   [wxMaxima: title   end   ] */\n"  
L"\n" 
L"\n" 
L"/* [wxMaxima: comment start ]\n"  + _(
L"Mathematics can be seen as a language consisting only of a rather small set of words and only a handful of grammatical rules. Both sets are carefully chosen, though, that by simply reformulating the question in many cases the answer can be found.\n" 
) + L"   [wxMaxima: comment end   ] */\n"  
L"\n" 
L"\n" 
L"/* [wxMaxima: comment start ]\n"  + _(
L"If you are studying mathematics the good news is that solving equations is an art a computer can help with in many special cases but that often relies on human creativity for finding algorithms and ways.\n" 
) + L"   [wxMaxima: comment end   ] */\n"  
L"\n" 
L"\n" 
L"/* [wxMaxima: section start ]\n"  + _(
L"Simple equations\n" 
) + L"   [wxMaxima: section end   ] */\n"  
L"\n" 
L"\n" 
L"/* [wxMaxima: comment start ]\n"  + _(
L"For many problems Maxima's solve() program instantly finds a list of solutions:\n" 
) + L"   [wxMaxima: comment end   ] */\n"  
L"\n" 
L"\n" 
L"/* [wxMaxima: input   start ] */\n" 
L"pyth:a^2+b^2=c^2;\n" 
L"/* [wxMaxima: input   end   ] */\n" 
L"\n" 
L"\n" 
L"/* [wxMaxima: input   start ] */\n" 
L"sol1:solve(pyth,a);\n" 
L"/* [wxMaxima: input   end   ] */\n" 
L"\n" 
L"\n" 
L"/* [wxMaxima: comment start ]\n"  + _(
L"Both solutions solve() offers are valied. We can pick any of them manually, for example:\n" 
) + L"   [wxMaxima: comment end   ] */\n"  
L"\n" 
L"\n" 
L"/* [wxMaxima: input   start ] */\n" 
L"sol2:sol1[2];\n" 
L"/* [wxMaxima: input   end   ] */\n" 
L"\n" 
L"\n" 
L"/* [wxMaxima: comment start ]\n"  + _(
L"By having given this equation a name (\"sol2\") we gained a way to re-use this equation later:\n" 
) + L"   [wxMaxima: comment end   ] */\n"  
L"\n" 
L"\n" 
L"/* [wxMaxima: input   start ] */\n" 
L"solve(sol2,b);\n" 
L"/* [wxMaxima: input   end   ] */\n" 
L"/* [wxMaxima: question  start ] */\n" 
L"Question #1\n" 
L"/* [wxMaxima: question  end   ] */\n" 
L"/* [wxMaxima: answer  start ] */\n" 
L"\n" 
L"/* [wxMaxima: answer  end   ] */\n" 
L"/* [wxMaxima: question  start ] */\n" 
L"<mth><st>Is </st><mi>a</mi><st> positive, negative or zero?</st></mth>\n" 
L"/* [wxMaxima: question  end   ] */\n" 
L"/* [wxMaxima: answer  start ] */\n" 
L"p;\n" 
L"/* [wxMaxima: answer  end   ] */\n" 
L"\n" 
L"\n" 
L"/* [wxMaxima: comment start ]\n"  + _(
L"This time the type of solutions we can get depends on the value of a which is why maxima asked about it. One can avoid asking questions by telling the answers to maxima's assume database:\n" 
) + L"   [wxMaxima: comment end   ] */\n"  
L"\n" 
L"\n" 
L"/* [wxMaxima: input   start ] */\n" 
L"assume(a>0, b>0, c>0);\n" 
L"solve(sol2,b);\n" 
L"/* [wxMaxima: input   end   ] */\n" 
L"\n" 
L"\n" 
L"/* [wxMaxima: comment start ]\n"  + _(
L"Solve also accepts lists as its arguments. In this case it will only find solutions if the following conditions are all met:\n" 
L" * The number of linearly independent equations matches the number of variables to solve to\n" 
L" * The solution doesn't completely change its form depending on the range one variable is in.\n" 
) + L"   [wxMaxima: comment end   ] */\n"  
L"\n" 
L"\n" 
L"/* [wxMaxima: input   start ] */\n" 
L"eq2:a+2*b=10;\n" 
L"solve(\n" 
L"    [pyth,eq2],\n" 
L"    [a,b]\n" 
L");\n" 
L"/* [wxMaxima: input   end   ] */\n" 
L"\n" 
L"\n" 
L"\n" 
L"/* Old versions of Maxima abort on loading files that end in a comment. */\n" 
L"\"Created with wxMaxima 19.12.4-DevelopmentSnapshot\"$\n" 
" ";
