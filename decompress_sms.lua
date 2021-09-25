EOF = -1
END_OF_STREAM = 256
ESCAPE = 257
SYMBOL_COUNT = 258
NODE_TABLE_COUNT = ( ( SYMBOL_COUNT * 2 ) - 1 )
ROOT_NODE = 0
MAX_WEIGHT = 0X8000
TRUE = 1
FALSE = 0

-- Node class
Node = {
	weight = 0,
	parent = -1,
	child_is_leaf = FALSE,
	child = -1
}

function Node:new(o)
	o = o or {}
	setmetatable(o, self)
	self.__index = self
	self.weight = 0
	self.parent = -1
	self.child_is_leaf = FALSE
	self.child = -1
	return o
end

-- Tree class
Tree = {
	leaf = {},
	next_free_node = -1,
	nodes = {}
}

function Tree:new(o)
	o = o or {}
	setmetatable(o, self)
	self.__index = self
	self.leaf = {}
	self.next_free_node = -1
	self.nodes = {}
	return o
end

function Tree:initialize()
	for i = 0, NODE_TABLE_COUNT - 1 do
		self.nodes[i] = Node:new()
	end

	self.nodes[ROOT_NODE].child = ESCAPE
	self.nodes[ROOT_NODE].child_is_leaf = TRUE
	self.nodes[ROOT_NODE].weight = 1
	self.nodes[ROOT_NODE].parent = -1
	self.leaf[ESCAPE] = ROOT_NODE

	self.next_free_node = ROOT_NODE + 1

	for i = 0, END_OF_STREAM - 1 do
		self.leaf[i] = -1
	end
end

-- DataStream class
DataStream = {
	byte_arr = {},
	current_byte_index = 1,
	mask = 0,
	rack = 0
}

function DataStream:new(o)
	o = o or {}
	setmetatable(o, self)
	self.__index = self
	self.byte_arr = {}
	self.current_byte_index = 1
	self.mask = 0x80
	self.rack = 0
	self.number_of_processed_bits = 0
	return o
end

function DataStream:GetBit()
	local value = 0;

	if #(self.byte_arr) == 0 then
		return value
	end

	if self.mask == 0x80 then
		self.rack = self.byte_arr[self.current_byte_index]
		self.current_byte_index = self.current_byte_index + 1
	end

	value = self.rack & self.mask
	self.mask = self.mask >> 1
	if self.mask == 0x00 then
		self.mask = 0x80
	end

	self.number_of_processed_bits = self.number_of_processed_bits + 1
	if value ~= 0 then
		return 1
	else
		return 0
	end
end

function DataStream:GetBits(bit_count)
	local mask
	local return_value = 0

	if bit_count == 0 then
		return return_value
	end

	mask = 1 << (bit_count - 1)
	return_value = 0

	while mask ~= 0 do
		if self.mask == 0x80 then
			self.rack = self.byte_arr[self.current_byte_index]
			self.current_byte_index = self.current_byte_index + 1
		end

		if (self.rack & self.mask) ~= 0 then
			return_value = return_value | mask
		end

		mask = mask >> 1
		self.mask = self.mask >> 1
		if self.mask == 0x00 then
			self.mask = 0x80
		end
	end

	self.number_of_processed_bits = self.number_of_processed_bits + bit_count
	return return_value
end

function Tree:DecodeSymbol(data_stream)
	local current_node
	local c
	local current_bit

	current_node = ROOT_NODE;
	while self.nodes[ current_node ].child_is_leaf == FALSE do
		current_bit = data_stream:GetBit()
		if current_bit == 1 then
			current_node = self.nodes[ current_node ].child;
		else
			current_node = self.nodes[ current_node ].child + 1;
		end
	end

	c = self.nodes[ current_node ].child;
	if c == ESCAPE then
		c = data_stream:GetBits(7)
		self:AddNewNode( c );
	end

	return c
end

function Tree:UpdateModel(c)
	local current_node;
	local new_node;

	if self.nodes[ ROOT_NODE].weight == MAX_WEIGHT then
		self.Rebuild();
	end

	current_node = self.leaf[ c ];
	while current_node ~= -1 do
		self.nodes[ current_node ].weight = self.nodes[current_node].weight + 1

		new_node = current_node
		for i = current_node, ROOT_NODE + 1, -1 do
			new_node = i
			if self.nodes[ new_node - 1 ].weight >= self.nodes[ current_node ].weight then
				break;
			end
		end

		if current_node ~= new_node then
			self:SwapNodes(current_node, new_node );
			current_node = new_node;
		end

		current_node = self.nodes[ current_node ].parent;
	end
end

-- Rebuilding the tree takes place when the counts have gone too
-- high.  From a simple point of view, rebuilding the tree just means
-- that we divide every count by two.  Unfortunately, due to truncation
-- effects, this means that the tree's shape might change.  Some nodes
-- might move up due to cumulative increases, while others may move
-- down.
function Tree:Rebuild()
	local i;
	local j;
	local k;
	local weight;

	-- To start rebuilding the table, I collect all the leaves of the
	-- Huffman tree and put them in the end of the tree.  While I am doing
	-- that, I scale the counts down by a factor of 2.
	j = self.next_free_node - 1;
	for i = j, ROOT_NODE, -1 do
		if self.nodes[ i ].child_is_leaf == TRUE then
			-- FIXME: This is reference
			self.nodes[ j ] = self.nodes[ i ];
			self.nodes[ j ].weight = ( self.nodes[ j ].weight + 1 ) / 2;
			j = j - 1 
		end
	end

	-- At this point, j points to the first free node.  I now have all the
	-- leaves defined, and need to start building the higher nodes on the
	-- tree.  I will start adding the new internal nodes at j.  Every time
	-- I add a new internal node to the top of the tree, I have to check
	-- to see where it really belongs in the tree.  It might stay at the
	-- top, but there is a good chance I might have to move it back down.
	-- If it does have to go down, I use the memmove() function to scoot
	-- everyone bigger up by one node.
	i = self.next_free_node - 2
	while j >= ROOT_NODE do
		k = i + 1;
		self.nodes[ j ].weight = self.nodes[ i ].weight + self.nodes[ k ].weight;
		weight = self.nodes[ j ].weight;
		self.nodes[ j ].child_is_leaf = FALSE;

		k = j + 1
		while weight < self.nodes[ k ].weight do
			k = k + 1
		end
		k = k - 1

		-- FIXME
		-- memmove( &self.nodes[ j ], &self.nodes[ j + 1 ], ( k - j ) * sizeof( struct node ) );

		self.nodes[ k ].weight = weight;
		self.nodes[ k ].child = i;
		self.nodes[ k ].child_is_leaf = FALSE;

		i = i - 2
		j = j - 1
	end

	-- The final step in tree reconstruction is to go through and set up
	-- all of the leaf and parent members.  This can be safely done now
	-- that every node is in its final position in the tree.
	for i = self.next_free_node - 1, ROOT_NODE, -1 do
		if self.nodes[ i ].child_is_leaf == TRUE then
			k = self.nodes[ i ].child;
			self.leaf[ k ] = i;
		else
			k = self.nodes[ i ].child;
			self.nodes[ k ].parent = i;
			self.nodes[ k + 1 ].parent = i;
		end
	end
end

-- Swapping nodes takes place when a node has grown too big for its
-- spot in the tree.  When swapping nodes i and j, we rearrange the
-- tree by exchanging the children under i with the children under j.
function Tree:SwapNodes( i, j )
	local temp = Node:new()

	if self.nodes [ i ].child_is_leaf == TRUE then
		self.leaf[ self.nodes[ i ].child ] = j;
	else
		self.nodes[ self.nodes[ i ].child ].parent = j;
		self.nodes[ self.nodes[ i ].child + 1 ].parent = j;
	end

	if self.nodes[ j ].child_is_leaf == TRUE then
		self.leaf[ self.nodes[ j ].child ] = i;
	else
		self.nodes[ self.nodes[ j ].child ].parent = i;
		self.nodes[ self.nodes[ j ].child + 1 ].parent = i;
	end

	CopyNodeByValue(self.nodes[i], temp)
	CopyNodeByValue(self.nodes[j], self.nodes[i])
	self.nodes[ i ].parent = temp.parent;
	temp.parent = self.nodes[ j ].parent;
	CopyNodeByValue(temp, self.nodes[j])
end

-- Adding a new node to the tree is pretty simple.  It is just a matter
-- of splitting the lightest-weight node in the tree, which is the
-- highest valued node.  We split it off into two new nodes, one of
-- which is the one being added to the tree.  We assign the new node a
-- weight of 0, so the tree doesn't have to be adjusted.  It will be
-- updated later when the normal update process occurs.  Note that this
-- code assumes that the lightest node has a leaf as a child.  If this
-- is not the case, the tree would be broken.
function Tree:AddNewNode(c )
	local lightest_node;
	local new_node;
	local zero_weight_node;

	lightest_node = self.next_free_node - 1;
	new_node = self.next_free_node;
	zero_weight_node = self.next_free_node + 1;
	self.next_free_node = self.next_free_node + 2;

	CopyNodeByValue(self.nodes[lightest_node], self.nodes[new_node])
	self.nodes[ new_node ].parent = lightest_node;
	self.leaf[ self.nodes[ new_node ].child ] = new_node;

	self.nodes[ lightest_node ].child = new_node;
	self.nodes[ lightest_node ].child_is_leaf = FALSE;

	self.nodes[ zero_weight_node ].child = c;
	self.nodes[ zero_weight_node ].child_is_leaf = TRUE;
	self.nodes[ zero_weight_node ].weight = 0;
	self.nodes[ zero_weight_node ].parent = lightest_node;
	self.leaf[ c ] = zero_weight_node;
end

function CopyNodeByValue(old_node, new_node)
	new_node.weight = old_node.weight
	new_node.parent = old_node.parent
	new_node.child_is_leaf = old_node.child_is_leaf
	new_node.child = old_node.child
end

local inp = assert(io.open(arg[1], "rb"))
local data = inp:read("*all")
inp:close()

local data_stream = DataStream:new()
for i = 1, #(data) do
	data_stream.byte_arr[i] = data:byte(i)
end

local tree = Tree:new()
tree:initialize()

local s = ""
local c = tree:DecodeSymbol(data_stream)
while data_stream.number_of_processed_bits <= 8 * #(data_stream.byte_arr) do
	s = s .. utf8.char(c)
	tree:UpdateModel(c)
	c = tree:DecodeSymbol(data_stream)
end
print(s)