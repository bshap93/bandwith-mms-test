require 'pry'
require 'pry-remote'
require 'pry-nav'

def kaprekar?(k)
  square = k**2
  square_str = square.to_s
  dig = square_str.length
  last_digits = last_digits = square_str[(square_str.length - k.to_s.length)..(square_str.length-1)].to_i
  first_digits = square_str[0..(square_str.length - (last_digits.to_s.length+1))].to_i
  sum = last_digits + first_digits
  binding.pry
  return sum == k
end


puts kaprekar?(297)