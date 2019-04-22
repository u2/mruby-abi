# copy some code from:
# https://github.com/cryptape/ruby-ethereum-abi/blob/master/lib/ethereum/abi.rb
# https://github.com/cryptape/ruby-rlp/blob/master/lib/rlp/utils.rb
# https://github.com/cryptape/ruby-ethereum/blob/master/lib/ethereum/utils.rb
# https://github.com/cryptape/ruby-ethereum/blob/master/lib/ethereum/constant.rb

module ABI
  BYTE_EMPTY = "".freeze
  BYTE_ZERO  = "\x00".freeze
  BYTE_ONE   = "\x01".freeze

  TT32   = 2**32
  TT40   = 2**40
  TT160  = 2**160
  TT256  = 2**256
  TT64M1 = 2**64 - 1

  UINT_MAX = 2**256 - 1
  UINT_MIN = 0
  INT_MAX  = 2**255 - 1
  INT_MIN  = -2**255

  HASH_ZERO = ("\x00"*32).freeze

  PUBKEY_ZERO = ("\x00"*32).freeze
  PRIVKEY_ZERO = ("\x00"*32).freeze
  PRIVKEY_ZERO_HEX = ('0'*64).freeze

  CONTRACT_CODE_SIZE_LIMIT = 0x6000

  class DecodingError < StandardError; end
  class ValueOutOfBounds < StandardError; end
  class EncodingError < StandardError; end

  class Type
    class <<self
      def size_type
        @size_type ||= new('uint', 256, [])
      end
    end
    attr :base, :sub, :dims

    ##
    # @param base [String] base name of type, e.g. uint for uint256[4]
    # @param sub  [String] subscript of type, e.g. 256 for uint256[4]
    # @param dims [Array[Integer]] dimensions of array type, e.g. [1,2,0]
    #   for uint256[1][2][], [] for non-array type
    #
    def initialize(base, sub, dims)
      @base = base
      @sub  = sub
      @dims = dims
    end

    def ==(another_type)
      base == another_type.base &&
        sub == another_type.sub &&
        dims == another_type.dims
    end

    ##
    # Get the static size of a type, or nil if dynamic.
    #
    # @return [Integer, NilClass]  size of static type, or nil for dynamic
    #   type
    #
    def size
      @size ||= if dims.empty?
                  if %w(string bytes).include?(base) && sub.empty?
                    nil
                  else
                    32
                  end
                else
                  if dims.last == 0 # 0 for dynamic array []
                    nil
                  else
                    subtype.dynamic? ? nil : dims.last * subtype.size
                  end
                end
    end

    def dynamic?
      size.nil?
    end

    ##
    # Type with one dimension lesser.
    #
    # @example
    #   Type.parse("uint256[2][]").subtype # => Type.new('uint', 256, [2])
    #
    # @return [Ethereum::ABI::Type]
    #
    def subtype
      @subtype ||= self.class.new(base, sub, dims[0...-1])
    end

  end

  def encode_abi(parsed_types, args)
    head_size = (0...args.size)
      .map {|i| parsed_types[i].size || 32 }
      .reduce(0, &:+)

    head, tail = '', ''
    args.each_with_index do |arg, i|
      if parsed_types[i].dynamic?
        head += encode_type(Type.size_type, head_size + tail.size)
        tail += encode_type(parsed_types[i], arg)
      else
        head += encode_type(parsed_types[i], arg)
      end
    end

    "#{head}#{tail}"
  end

  ##
  # Encodes a single value (static or dynamic).
  #
  # @param type [Ethereum::ABI::Type] value type
  # @param arg [Object] value
  #
  # @return [String] encoded bytes
  #
  def encode_type(type, arg)
    if %w(string bytes).include?(type.base) && type.sub.empty?
      raise ArgumentError, "arg must be a string" unless arg.instance_of?(String)

      size = encode_type Type.size_type, arg.size
      padding = BYTE_ZERO * (Utils.ceil32(arg.size) - arg.size)

      "#{size}#{arg}#{padding}"
    elsif type.dynamic?
      raise ArgumentError, "arg must be an array" unless arg.instance_of?(Array)

      head, tail = '', ''
      if type.dims.last == 0
        head += encode_type(Type.size_type, arg.size)
      else
        raise ArgumentError, "Wrong array size: found #{arg.size}, expecting #{type.dims.last}" unless arg.size == type.dims.last
      end

      sub_type = type.subtype
      sub_size = type.subtype.size
      arg.size.times do |i|
        if sub_size.nil?
          head += encode_type(Type.size_type, 32*arg.size + tail.size)
          tail += encode_type(sub_type, arg[i])
        else
          head += encode_type(sub_type, arg[i])
        end
      end

      "#{head}#{tail}"
    else # static type
      if type.dims.empty?
        encode_primitive_type type, arg
      else
        arg.map {|x| encode_type(type.subtype, x) }.join
      end
    end
  end

  def encode_primitive_type(type, arg)  
    case type.base
    when 'uint'
      real_size = type.sub.to_i
      i = get_uint arg

      raise ValueOutOfBounds, arg unless i >= 0 && i < 2**real_size
      Utils.zpad_int i
    when 'bool'
      raise ArgumentError, "arg is not bool: #{arg}" unless arg.instance_of?(TrueClass) || arg.instance_of?(FalseClass)
      Utils.zpad_int(arg ? 1: 0)
    when 'int'
      real_size = type.sub.to_i
      i = get_int arg

      raise ValueOutOfBounds, arg unless i >= -2**(real_size-1) && i < 2**(real_size-1)
      if i < 0
        i = i + 2**type.sub.to_i
      end

      Utils.zpad_int(i)
    when 'ureal', 'ufixed'
      high, low = type.sub.split('x').map(&:to_i)

      raise ValueOutOfBounds, arg unless arg >= 0 && arg < 2**high
      Utils.zpad_int((arg * 2**low).to_i)
    when 'real', 'fixed'
      high, low = type.sub.split('x').map(&:to_i)

      raise ValueOutOfBounds, arg unless arg >= -2**(high - 1) && arg < 2**(high - 1)

      i = (arg * 2**low).to_i

      if i < 0
        i = i + 2**(high+low)
      end

      Utils.zpad_int(i)
    when 'string', 'bytes'
      raise EncodingError, "Expecting string: #{arg}" unless arg.instance_of?(String)

      if type.sub.empty? # variable length type
        size = Utils.zpad_int arg.size
        padding = BYTE_ZERO * (Utils.ceil32(arg.size) - arg.size)
        "#{size}#{arg}#{padding}"
      else # fixed length type
        raise ValueOutOfBounds, arg unless arg.size <= type.sub.to_i

        padding = BYTE_ZERO * (32 - arg.size)
        "#{arg}#{padding}"
      end
    when 'hash'
      size = type.sub.to_i
      raise EncodingError, "too long: #{arg}" unless size > 0 && size <= 32

      if arg.is_a?(Integer)
        Utils.zpad_int(arg)
      elsif arg.size == size
        Utils.zpad arg, 32
      elsif arg.size == size * 2
        Utils.zpad_hex arg
      else
        raise EncodingError, "Could not parse hash: #{arg}"
      end
    when 'address'
      if arg.is_a?(Integer)
        Utils.zpad_int arg
      elsif arg.size == 20
        Utils.zpad arg, 32
      elsif arg.size == 40
        Utils.zpad_hex arg
      elsif arg.size == 42 && arg[0,2] == '0x'
        Utils.zpad_hex arg[2..-1]
      else
        raise EncodingError, "Could not parse address: #{arg}"
      end
    else
      raise EncodingError, "Unhandled type: #{type.base} #{type.sub}"
    end
  end

  ##
  # Decodes multiple arguments using the head/tail mechanism.
  #
  def decode_abi(parsed_types, types, data)
    outputs = [nil] * types.size
    start_positions = [nil] * types.size + [data.size]

    # TODO: refactor, a reverse iteration will be better
    pos = 0
    parsed_types.each_with_index do |t, i|
      # If a type is static, grab the data directly, otherwise record its
      # start position
      if t.dynamic?
        start_positions[i] = Utils.big_endian_to_int(data[pos, 32])

        j = i - 1
        while j >= 0 && start_positions[j].nil?
          start_positions[j] = start_positions[i]
          j -= 1
        end

        pos += 32
      else
        outputs[i] = data[pos, t.size]
        pos += t.size
      end
    end

    # We add a start position equal to the length of the entire data for
    # convenience.
    j = types.size - 1
    while j >= 0 && start_positions[j].nil?
      start_positions[j] = start_positions[types.size]
      j -= 1
    end

    raise DecodingError, "Not enough data for head" unless pos <= data.size

    parsed_types.each_with_index do |t, i|
      if t.dynamic?
        offset, next_offset = start_positions[i, 2]
        outputs[i] = data[offset...next_offset]
      end
    end

    parsed_types.zip(outputs).map {|type, out| decode_type(type, out) }
  end

  def decode_type(type, arg)
    if %w(string bytes).include?(type.base) && type.sub.empty?
      l = Utils.big_endian_to_int arg[0,32]
      data = arg[32..-1]

      raise DecodingError, "Wrong data size for string/bytes object" unless data.size == Utils.ceil32(l)

      data[0, l]
    elsif type.dynamic?
      l = Utils.big_endian_to_int arg[0,32]
      subtype = type.subtype

      if subtype.dynamic?
        raise DecodingError, "Not enough data for head" unless arg.size >= 32 + 32*l

        start_positions = (1..l).map {|i| Utils.big_endian_to_int arg[32*i, 32] }
        start_positions.push arg.size

        outputs = (0...l).map {|i| arg[start_positions[i]...start_positions[i+1]] }

        outputs.map {|out| decode_type(subtype, out) }
      else
        (0...l).map {|i| decode_type(subtype, arg[32 + subtype.size*i, subtype.size]) }
      end
    elsif !type.dims.empty? # static-sized arrays
      l = type.dims.last[0]
      subtype = type.subtype

      (0...l).map {|i| decode_type(subtype, arg[subtype.size*i, subtype.size]) }
    else
      decode_primitive_type type, arg
    end
  end

  def decode_primitive_type(type, data)
    case type.base
    when 'address'
      Utils.encode_hex data[12..-1]
    when 'string', 'bytes'
      if type.sub.empty? # dynamic
        size = Utils.big_endian_to_int data[0,32]
        data[32..-1][0,Integer(size.to_s)]
      else # fixed
        data[0, Integer(type.sub)]
      end
    when 'hash'
      data[(32 - Integer(type.sub)), Integer(type.sub)]
    when 'uint'
      Utils.big_endian_to_int data
    when 'int'
      u = Utils.big_endian_to_int data
      u >= 2**(type.sub.to_i-1) ? (u - 2**type.sub.to_i) : u
    when 'ureal', 'ufixed'
      high, low = type.sub.split('x').map(&:to_i)
      Utils.big_endian_to_int(data) * 1.0 / 2**low
    when 'real', 'fixed'
      high, low = type.sub.split('x').map(&:to_i)
      u = Utils.big_endian_to_int data
      i = u >= 2**(high+low-1) ? (u - 2**(high+low)) : u
      i * 1.0 / 2**low
    when 'bool'
      data[-1] == BYTE_ONE
    else
      raise DecodingError, "Unknown primitive type: #{type.base}"
    end
  end

  private

  def get_uint(n)
    case n
    when Integer
      raise EncodingError, "Number out of range: #{n}" if n > UINT_MAX || n < UINT_MIN
      n
    when String
      if n.size == 40
        Utils.big_endian_to_int Utils.decode_hex(n)
      elsif n.size <= 32
        Utils.big_endian_to_int n
      else
        raise EncodingError, "String too long: #{n}"
      end
    when true
      1
    when false, nil
      0
    else
      raise EncodingError, "Cannot decode uint: #{n}"
    end
  end

  def get_int(n)
    case n
    when Integer
      raise EncodingError, "Number out of range: #{n}" if n > INT_MAX || n < INT_MIN
      n
    when String
      if n.size == 40
        i = Utils.big_endian_to_int Utils.decode_hex(n)
      elsif n.size <= 32
        i = Utils.big_endian_to_int n
      else
        raise EncodingError, "String too long: #{n}"
      end
      i > INT_MAX ? (i-TT256) : i
    when true
      1
    when false, nil
      0
    else
      raise EncodingError, "Cannot decode int: #{n}"
    end
  end
end

module Utils
  extend self
  def primitive?(item)
    item.instance_of?(String)
  end

  def list?(item)
    !primitive?(item) && item.respond_to?(:each)
  end

  def bytes_to_str(v)
    v.unpack('U*').pack('U*')
  end

  def str_to_bytes(v)
    bytes?(v) ? v : v.b
  end

  def big_endian_to_int(v)
    v.unpack('H*').first.to_i(16)
  end

  def int_to_big_endian(v)
    hex = v.to_s(16)
    if hex.size % 2 == 1
      hex = "0#{hex}"
    end
    [hex].pack('H*')
  end

  def encode_hex(b)
    raise TypeError, "Value must be an instance of String" unless b.instance_of?(String)
    b.unpack("H*").first
  end

  def decode_hex(s)
    raise TypeError, "Value must be an instance of string" unless s.instance_of?(String)
    # raise TypeError, 'Non-hexadecimal digit found' unless s =~ /\A[0-9a-fA-F]*\z/
    [s].pack("H*")
  end

  BINARY_ENCODING = 'ASCII-8BIT'.freeze
  def bytes?(s)
    s && s.instance_of?(String) && s.encoding.name == BINARY_ENCODING
  end

  def ceil32(x)
    x % 32 == 0 ? x : (x + 32 - x%32)
  end

  def lpad(x, symbol, l)
    return x if x.size >= l
    symbol * (l - x.size) + x
  end

  def rpad(x, symbol, l)
    return x if x.size >= l
    x + symbol * (l - x.size)
  end

  def zpad(x, l)
    lpad x, BYTE_ZERO, l
  end

  def zunpad(x)
    x.sub /\A\x00+/, ''
  end

  def zpad_int(n, l=32)
    zpad encode_int(n.to_i), l
  end

  def zpad_hex(s, l=32)
    zpad decode_hex(s), l
  end

  def int_to_addr(x)
    zpad_int x, 20
  end

  def encode_int(n)
    raise ArgumentError, "Integer invalid or out of range: #{n} #{n.class}" unless (n.is_a?(Integer) || n.is_a?(Bignum)) && n >= 0 && n <= UINT_MAX
    int_to_big_endian n
  end

  def decode_int(v)
    raise ArgumentError, "No leading zero bytes allowed for integers" if v.size > 0 && (v[0] == Constant::BYTE_ZERO || v[0] == 0)
    big_endian_to_int v
  end

  def bytearray_to_int(arr)
    o = 0
    arr.each {|x| o = (o << 8) + x }
    o
  end

  def int_array_to_bytes(arr)
    arr.pack('C*')
  end

  def bytes_to_int_array(bytes)
    bytes.unpack('C*')
  end

  def coerce_to_int(x)
    if x.is_a?(Numeric)
      x
    elsif x.size == 40
      big_endian_to_int decode_hex(x)
    else
      big_endian_to_int x
    end
  end

  def coerce_to_bytes(x)
    if x.is_a?(Numeric)
      int_to_big_endian x
    elsif x.size == 40
      decode_hex(x)
    else
      x
    end
  end
end
