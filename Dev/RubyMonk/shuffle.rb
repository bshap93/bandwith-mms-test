def number_shuffle(number)
  digit_array = number.to_s.split('').to_a
  permutations = factorial(digit_array.length)
  num_array = []
  while num_array.length < permutations
    num_array << digit_array.shuffle.join('').to_i
    num_array.uniq!
  end
  return num_array.map {|num_string| 
end

def factorial(i)
  if i == 1
    return 1
  else
    return i * factorial(i-1)
  end
end

number_shuffle(123)