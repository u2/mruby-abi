include ABI
include Utils

assert("test_use_abi_class_methods") do
  assert_equal encode_abi([Type.new('int', '256', [])], [1]), encode_abi([Type.new('int', '256', [])], [1])
end

assert("test_abi_encode_var_sized_array") do
  bytes = "\x00" * 32 * 3
  assert_equal "#{zpad_int(32)}#{zpad_int(3)}#{bytes}", encode_abi([Type.new('address', '', [0])], [["\x00" * 20]*3])
end

assert("test_abi_encode_fixed_sized_array") do
  assert_equal "#{zpad_int(5)}#{zpad_int(6)}", encode_abi([Type.new('uint', '16', [2])], [[5,6]])
end

assert("test_abi_encode_signed_int") do
  assert_equal 1,  decode_abi([Type.new('int', '8', [])], "int8", encode_abi([Type.new('int', '8', [])], [1]))[0]
  assert_equal -1, decode_abi([Type.new('int', '8', [])], "int8", encode_abi([Type.new('int', '8', [])], [-1]))[0]
end

assert("test_abi_encode_primitive_type") do
  type = Type.new('bool', '', [])
  assert_equal zpad_int(1), encode_primitive_type(type, true)
  assert_equal zpad_int(0), encode_primitive_type(type, false)

  type = Type.new('uint', '8', [])
  assert_equal zpad_int(255), encode_primitive_type(type, 255)
  assert_raise(ValueOutOfBounds) { encode_primitive_type(type, 256) }

  type = Type.new('int', '8', [])
  assert_equal zpad("\x80", 32), encode_primitive_type(type, -128)
  assert_equal zpad("\x7f", 32), encode_primitive_type(type, 127)
  assert_raise(ValueOutOfBounds) { encode_primitive_type(type, -129) }
  assert_raise(ValueOutOfBounds) { encode_primitive_type(type, 128) }

  type = Type.new('ureal', '128x128', [])
  assert_equal ("\x00"*32), encode_primitive_type(type, 0)
  assert_equal ("\x00"*15 + "\x01\x20" + "\x00"*15), encode_primitive_type(type, 1.125)
  assert_equal ("\x7f" + "\xff"*15 + "\x00"*16), encode_primitive_type(type, 2**127-1)

  type = Type.new('real', '128x128', [])
  assert_equal ("\xff"*16 + "\x00"*16), encode_primitive_type(type, -1)
  assert_equal ("\x80" + "\x00"*31), encode_primitive_type(type, -2**127)
  assert_equal ("\x7f" + "\xff"*15 + "\x00"*16), encode_primitive_type(type, 2**127-1)
  assert_equal "#{zpad_int(1, 16)}\x20#{"\x00"*15}", encode_primitive_type(type, 1.125)
  assert_equal "#{"\xff"*15}\xfe\xe0#{"\x00"*15}", encode_primitive_type(type, -1.125)
  assert_raise(ValueOutOfBounds) { encode_primitive_type(type, -2**127 - 1) }
  assert_raise(ValueOutOfBounds) { encode_primitive_type(type, 2**127) }

  type = Type.new('bytes', '', [])
  assert_equal "#{zpad_int(3)}\x01\x02\x03#{"\x00"*29}", encode_primitive_type(type, "\x01\x02\x03")

  type = Type.new('bytes', '8', [])
  assert_equal "\x01\x02\x03#{"\x00"*29}", encode_primitive_type(type, "\x01\x02\x03")

  type = Type.new('hash', '32', [])
  assert_equal ("\xff"*32), encode_primitive_type(type, "\xff"*32)
  assert_equal ("\xff"*32), encode_primitive_type(type, "ff"*32)

  type = Type.new('address', '', [])
  assert_equal zpad("\xff"*20, 32), encode_primitive_type(type, "\xff"*20)
  assert_equal zpad("\xff"*20, 32), encode_primitive_type(type, "ff"*20)
  assert_equal zpad("\xff"*20, 32), encode_primitive_type(type, "0x"+"ff"*20)
end

assert("test_abi_decode_primitive_type") do
  type = Type.new('address', '', [])
  assert_equal 'ff'*20, decode_primitive_type(type, encode_primitive_type(type, "0x"+"ff"*20))

  type = Type.new('bytes', '', [])
  assert_equal "\x01\x02\x03", decode_primitive_type(type, encode_primitive_type(type, "\x01\x02\x03"))

  type = Type.new('bytes', '8', [])
  assert_equal ("\x01\x02\x03"+"\x00"*5), decode_primitive_type(type, encode_primitive_type(type, "\x01\x02\x03"))

  type = Type.new('hash', '20', [])
  assert_equal ("\xff"*20), decode_primitive_type(type, encode_primitive_type(type, "ff"*20))

  type = Type.new('uint', '8', [])
  assert_equal 0, decode_primitive_type(type, encode_primitive_type(type, 0))
  assert_equal 255, decode_primitive_type(type, encode_primitive_type(type, 255))

  type = Type.new('int', '8', [])
  assert_equal -128, decode_primitive_type(type, encode_primitive_type(type, -128))
  assert_equal 127, decode_primitive_type(type, encode_primitive_type(type, 127))

  type = Type.new('ureal', '128x128', [])
  assert_equal 0, decode_primitive_type(type, encode_primitive_type(type, 0))
  assert_equal 125.125, decode_primitive_type(type, encode_primitive_type(type, 125.125))
  assert_equal (2**128-1).to_f, decode_primitive_type(type, encode_primitive_type(type, 2**128-1))

  type = Type.new('real', '128x128', [])
  assert_equal 1, decode_primitive_type(type, encode_primitive_type(type, 1))
  assert_equal -1, decode_primitive_type(type, encode_primitive_type(type, -1))
  assert_equal 125.125, decode_primitive_type(type, encode_primitive_type(type, 125.125))
  assert_equal -125.125, decode_primitive_type(type, encode_primitive_type(type, -125.125))
  assert_equal (2**127-1).to_f, decode_primitive_type(type, encode_primitive_type(type, 2**127-1))
  assert_equal -2**127, decode_primitive_type(type, encode_primitive_type(type, -2**127))

  type = Type.new('bool', '', [])
  assert_equal true, decode_primitive_type(type, encode_primitive_type(type, true))
  assert_equal false, decode_primitive_type(type, encode_primitive_type(type, false))
end