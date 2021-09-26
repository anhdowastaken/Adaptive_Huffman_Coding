-- Const
EOF              = -1
END_OF_STREAM    = 256
ESCAPE           = 257
SYMBOL_COUNT     = 258
NODE_TABLE_COUNT = ((SYMBOL_COUNT * 2) - 1)
ROOT_NODE        = 0
MAX_WEIGHT       = 0X8000

TRUE  = 1
FALSE = 0

-- DataStream
DataStream = {
	byte_arr = {},
	current_byte_index = 1,
	mask = 0,
	rack = 0
}

function DataStream:New(o)
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
	local value = 0

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

-- Node
Node = {
	weight = 0,
	parent = -1,
	child_is_leaf = FALSE,
	child = -1
}

function Node:New(o)
	o = o or {}
	setmetatable(o, self)
	self.__index = self
	self.weight = 0
	self.parent = -1
	self.child_is_leaf = FALSE
	self.child = -1
	return o
end

function CopyNodeByValue(old_node, new_node)
	new_node.weight = old_node.weight
	new_node.parent = old_node.parent
	new_node.child_is_leaf = old_node.child_is_leaf
	new_node.child = old_node.child
end

-- Tree
Tree = {
	leaf = {},
	next_free_node = -1,
	nodes = {}
}

function Tree:New(o)
	o = o or {}
	setmetatable(o, self)
	self.__index = self
	self.leaf = {}
	self.next_free_node = -1
	self.nodes = {}
	return o
end

function Tree:Initialize()
	for i = 0, NODE_TABLE_COUNT - 1 do
		self.nodes[i] = Node:New()
	end

	self.nodes[ROOT_NODE].child = ESCAPE
	self.nodes[ROOT_NODE].child_is_leaf = TRUE
	self.nodes[ROOT_NODE].weight = 1
	self.nodes[ROOT_NODE].parent = -1
	self.leaf[ESCAPE] = ROOT_NODE

	self.next_free_node = ROOT_NODE + 1

	for i = 0, SYMBOL_COUNT - 1 do
		self.leaf[i] = -1
	end
end

function Tree:DecodeSymbol(data_stream)
	local current_node
	local c
	local current_bit

	current_node = ROOT_NODE
	while self.nodes[current_node].child_is_leaf == FALSE do
		current_bit = data_stream:GetBit()
		if current_bit == 1 then
			current_node = self.nodes[current_node].child
		else
			current_node = self.nodes[current_node].child + 1
		end
	end

	c = self.nodes[current_node].child
	if c == ESCAPE then
		c = data_stream:GetBits(7)
		self:AddNewNode(c)
	end

	return c
end

function Tree:UpdateModel(c)
	local current_node
	local new_node

	if self.nodes[ROOT_NODE].weight >= MAX_WEIGHT then
		self:Rebuild()
	end

	current_node = self.leaf[c]
	while current_node ~= -1 do
		self.nodes[current_node].weight = self.nodes[current_node].weight + 1

		new_node = current_node
		for i = current_node, ROOT_NODE + 1, -1 do
			new_node = i
			if self.nodes[new_node - 1].weight >= self.nodes[current_node].weight then
				break
			end
		end

		if current_node ~= new_node then
			self:SwapNodes(current_node, new_node)
			current_node = new_node
		end

		current_node = self.nodes[current_node].parent
	end
end

-- Rebuilding the tree takes place when the counts have gone too
-- high.  From a simple point of view, rebuilding the tree just means
-- that we divide every count by two.  Unfortunately, due to truncation
-- effects, this means that the tree's shape might change.  Some nodes
-- might move up due to cumulative increases, while others may move
-- down.
function Tree:Rebuild()
	local i
	local j
	local k
	local weight

	-- To start rebuilding the table, I collect all the leaves of the
	-- Huffman tree and put them in the end of the tree.  While I am doing
	-- that, I scale the counts down by a factor of 2.
	j = self.next_free_node - 1
	i = j
	while i >= ROOT_NODE do
		if self.nodes[i].child_is_leaf == TRUE then
			CopyNodeByValue(self.nodes[i], self.nodes[j])
			self.nodes[j].weight = math.floor((self.nodes[j].weight + 1) / 2)
			j = j - 1
		end
		i = i - 1
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
		k = i + 1
		self.nodes[j].weight = self.nodes[i].weight + self.nodes[k].weight
		weight = self.nodes[j].weight
		self.nodes[j].child_is_leaf = FALSE

		k = j + 1
		while weight < self.nodes[k].weight do
			k = k + 1
		end
		k = k - 1

		local tmp = {}
		for m = 0, (k - j) - 1 do
			tmp[m] = Node:New()
		end
		-- Copy src to tmp
		for m = j + 1, k do
			CopyNodeByValue(self.nodes[m], tmp[m - (j + 1)])
		end
		-- Copy tmp to dest
		for m = j, k - 1 do
			CopyNodeByValue(tmp[m - j], self.nodes[m])
		end

		self.nodes[k].weight = weight
		self.nodes[k].child = i
		self.nodes[k].child_is_leaf = FALSE

		i = i - 2
		j = j - 1
	end

	-- The final step in tree reconstruction is to go through and set up
	-- all of the leaf and parent members.  This can be safely done now
	-- that every node is in its final position in the tree.
	i = self.next_free_node - 1
	while i >= ROOT_NODE do
		if self.nodes[i].child_is_leaf == TRUE then
			k = self.nodes[i].child
			self.leaf[k] = i
		else
			k = self.nodes[i].child
			self.nodes[k].parent = i
			self.nodes[k + 1].parent = i
		end
		i = i - 1
	end
end

-- Swapping nodes takes place when a node has grown too big for its
-- spot in the tree.  When swapping nodes i and j, we rearrange the
-- tree by exchanging the children under i with the children under j.
function Tree:SwapNodes(i, j)
	local temp = Node:New()

	if self.nodes [i].child_is_leaf == TRUE then
		self.leaf[self.nodes[i].child] = j
	else
		self.nodes[self.nodes[i].child].parent = j
		self.nodes[self.nodes[i].child + 1].parent = j
	end

	if self.nodes[j].child_is_leaf == TRUE then
		self.leaf[self.nodes[j].child] = i
	else
		self.nodes[self.nodes[j].child].parent = i
		self.nodes[self.nodes[j].child + 1].parent = i
	end

	CopyNodeByValue(self.nodes[i], temp)
	CopyNodeByValue(self.nodes[j], self.nodes[i])
	self.nodes[i].parent = temp.parent
	temp.parent = self.nodes[j].parent
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
function Tree:AddNewNode(c)
	local lightest_node
	local new_node
	local zero_weight_node

	lightest_node = self.next_free_node - 1
	new_node = self.next_free_node
	zero_weight_node = self.next_free_node + 1
	self.next_free_node = self.next_free_node + 2

	CopyNodeByValue(self.nodes[lightest_node], self.nodes[new_node])
	self.nodes[new_node].parent = lightest_node
	self.leaf[self.nodes[new_node].child] = new_node

	self.nodes[lightest_node].child = new_node
	self.nodes[lightest_node].child_is_leaf = FALSE

	self.nodes[zero_weight_node].child = c
	self.nodes[zero_weight_node].child_is_leaf = TRUE
	self.nodes[zero_weight_node].weight = 0
	self.nodes[zero_weight_node].parent = lightest_node
	self.leaf[c] = zero_weight_node
end

-- This array is used to keep track of all the nodes that are in a given
-- row.  The nodes are kept in a linked list.  This array is used to keep
-- track of the first member.  The subsequent members will be found in
-- a linked list in the positions[] array.
Row = {
	first_member = 0,
	count = 0
}

function Row:New(o)
	o = o or {}
	setmetatable(o, self)
	self.__index = self
	self.first_member = 0
	self.count = 0
	return o
end

rows = {}
for i = 0, 31 do
	rows[i] = Row:New()
end

-- The positions[] array is used to keep track of the row and column of each
-- node in the tree.  The next_member element points to the next node
-- in the row for the given node.  The column is calculated on the fly,
-- and represents the actual column that a given number is to be printed in.
-- Note that the column for a node is not an actual column on the page.  For
-- purposes of analysis, it is assumed that each node takes up exactly one
-- column.  So, if printing out the actual values in a node takes up for
-- spaces on the printed page, we might want to allocate five physical print
-- columns for each column in the array.
Location = {
	row = 0,
	next_member = 0,
	column = 0
}

function Location:New(o)
	o = o or {}
	setmetatable(o, self)
	self.__index = self
	self.row = 0
	self.next_member = 0
	self.column = 0
	return o
end

positions = {}
for i = 0, NODE_TABLE_COUNT do
	positions[i] = Location:New()
end

-- This is the main routine called to print out a Huffman tree.  It first
-- calls the PrintCodes function, which prints out the binary codes
-- for each symbol.  After that, it calculates the row and column that
-- each node will be printed in, then prints the tree out.  This code
-- is not documented in the book, since it is essentially irrelevant to
-- the data compression process.  However, it is nice to be able to
-- print out the tree.
function Tree:PrintTree()
	local min

	self:PrintCodes()
	for i = 0, 31 do
		rows[i].count = 0
		rows[i].first_member = -1
	end
	self:CalculateRows(ROOT_NODE, 0)
	self:CalculateColumns(ROOT_NODE, 0)

	min = self:FindMinimumColumn(ROOT_NODE, 31)
	RescaleColumns(min)
	self:Print(0, 31)
end

-- In order to print out the tree, I need to calculate the row and column
-- where each node will be printed.  The rows are easier than the columns,
-- and I do them first.  It is easy to keep track of what row a node is
-- in as I walk through the tree.  As I walk through the tree, I also keep
-- track of the order the nodes appear in a given row, by adding them to
-- a linked list in the proper order.  After calculate_rows() has been
-- recursively called all the way through the tree, I have a linked list of
-- nodes for each row.  This same linked list is used later to calculate
-- which column each node appears in.
function Tree:CalculateRows(node, level)
	if rows[level].first_member == -1 then
		rows[level].first_member = node
		rows[level].count = 0
		positions[node].row = level
		positions[node].next_member = -1
	else
		positions[node].row = level
		positions[node].next_member = rows[level].first_member
		rows[level].first_member = node
		rows[level].count = rows[level].count + 1
	end

	if self.nodes[node].child_is_leaf == FALSE then
		self:CalculateRows(self.nodes[node].child, level + 1)
		self:CalculateRows(self.nodes[node].child + 1, level + 1)
	end
end

-- After I know which row each of the nodes is in, I can start the
-- hard work, which is calculating the columns.  This routine gets
-- called recursively.  It starts off with a starting guess for where
-- we want the node to go, and returns the actual result, which is
-- the column the node ended up in.  For example, I might want my node
-- to print in column 0.  After recursively evaluating everything under
-- the node, I may have been pushed over to node -10 (the tree is
-- evaluated down the right side first).  I return that to whoever called
-- this routine so it can use the nodes position to calculate where
-- the node in a higher row is to be placed.
function Tree:CalculateColumns(node, starting_guess)
	local next_node
	local right_side
	local left_side

	-- The first thing I check is to see if the node on my immediate right has
	-- already been placed.  If it has, I need to make sure that I am at least
	-- 4 columns to the right of it.  This allows me to print 3 characters plus
	-- leave a blank space between us.
	next_node = positions[node].next_member
	if next_node ~= -1 then
		if positions[next_node].column < (starting_guess + 4) then
			starting_guess = positions[next_node].column - 4
		end
	end

	if self.nodes[node].child_is_leaf == TRUE then
		positions[node].column = starting_guess
		return starting_guess
	end

	-- After I have adjusted my starting guess, I calculate the actual position
	-- of the right subtree of this node.  I pass it a guess for a starting
	-- node based on my starting guess.  Naturally, what comes back may be
	-- moved over quite a bit.
	right_side = self:CalculateColumns(self.nodes[node].child, starting_guess + 2)

	-- After figuring out where the right side lands, I do the same for the
	-- left side.  After doing the right side, I have a pretty good guess where
	-- the starting column for the left side might go, so I can pass it a good
	-- guess for a starting column.
	left_side = self:CalculateColumns(self.nodes[node].child + 1, right_side - 4)

	-- Once I know where the starting column for the left and right subtrees
	-- are going to be for sure, I know where this node should go, which is
	-- right in the middle between the two.  I calcluate the column, store it,
	-- then return the result to whoever called me.
	starting_guess = math.floor((right_side + left_side) / 2)
	positions[node].column = starting_guess

	return starting_guess
end

function Tree:FindMinimumColumn(node, max_row)
	local min_right
	local min_left

	if self.nodes[node].child_is_leaf == TRUE or max_row == 0 then
		return positions[node].column
	end

	max_row = max_row - 1
	min_right = self:FindMinimumColumn(self.nodes[node].child + 1, max_row)
	min_left = self:FindMinimumColumn(self.nodes[node].child, max_row)
	if min_right < min_left then
		return min_right
	else
		return min_left
	end
end

-- Once the columns of each node have been calculated, I go back and rescale
-- the columns to be actual printer columns.  In this particular program,
-- each node takes three characters to print, plus one space to keep nodes
-- separate.  We take advantage of the fact that every node has at least one
-- logical column between it and the ajacent node, meaning that we can space
-- nodes only two physical columns apart.  The spacing here consists of
-- rescaling each column so that the smallest column is at zero, then
-- multiplying by two to get a physical printer column.
function RescaleColumns(factor)
	local i
	local node

	-- Once min is known, we can rescale the tree so that column min is
	-- pushed over to column 0, and each logical column is set to be two
	-- physical columns on the printer.
	for i = 0, 29 do
		if rows[i].first_member == -1 then
			break
		end

		node = rows[i].first_member
		positions[node].column = positions[node].column - factor
		node = positions[node].next_member

		while node ~= -1 do
			positions[node].column = positions[node].column - factor
			node = positions[node].next_member
		end
	end
end

function IsCharacterPrintable(c)
	if c > 0x1F and c < 0x7F then
		return TRUE
	else
		return FALSE
	end
end

-- This routine is called to print out the Huffman code for each symbol.
-- The real work is done by the PrintCode routine, which racks up the
-- bits and puts them out in the right order.
function Tree:PrintCodes()
	print("")
	for i = 0, SYMBOL_COUNT - 1 do
		if self.leaf[i] ~= -1 then
			local buffer = ""

			if IsCharacterPrintable(i) == TRUE then
				buffer = buffer .. string.format("%5c: ", i)
			else
				buffer = buffer .. string.format("<%3d>: ", i)
			end
			buffer = buffer .. string.format("%5u", self.nodes[self.leaf[i]].weight)
			buffer = buffer .. " "
			buffer = buffer .. self:PrintCode(i)

			print(buffer)
		end
	end
	print("")
end

-- PrintCode is a workhorse routine that prints out the Huffman code for
-- a given symbol.  It ends up looking a lot like EncodeSymbol(), since
-- it more or less has to do the same work.  The major difference is that
-- instead of calling OutputBit, this routine calls putc, with a character
-- argument.
function Tree:PrintCode(c)
	local code
	local current_bit
	local code_size
	local current_node
	local buffer = ""

	code = 0
	current_bit = 1
	code_size = 0
	current_node = self.leaf[c]
	while current_node ~= ROOT_NODE do
		if current_node % 2 == 1 then
			code = code | current_bit
		end
		current_bit = current_bit << 1
		code_size = code_size + 1
		current_node = self.nodes[current_node].parent
	end

	for i = 0, code_size - 1 do
		current_bit = current_bit >> 1
		if code & current_bit ~= 0 then
			buffer = buffer .. "1"
		else
			buffer = buffer .. "0"
		end
	end

	return buffer
end

-- print_tree is called after the row and column of each node have been
-- calculated.  It just calls the four workhorse routines that are
-- responsible for printing out the four elements that go on each row.
-- At the top of the row are the connecting lines hooking the tree
-- together.  On the next line of the row are the node numbers.  Below
-- them are the weights, and finally the symbol, if there is one.
function Tree:Print(first_row, last_row)
	for row = first_row, last_row do
		if rows[row].first_member == -1 then
			break
		end

		if row > first_row then
			self:PrintConnectingLines(row)
		end

		PrintNodeNumbers(row)
		self:PrintWeights(row)
		self:PrintSymbol(row)
	end
end

LEFT_END  = "+"
RIGHT_END = "+"
CENTER    = "+"
LINE      = "-"
VERTICAL  = "|"

function Tree:PrintConnectingLines(row)
	local current_col
	local start_col
	local end_col
	local center_col
	local node
	local parent
	local buffer = ""

	current_col = 0
	node = rows[row].first_member
	while node ~= -1 do
		start_col = positions[node].column + 2
		node = positions[node].next_member
		end_col = positions[node].column + 2
		parent = self.nodes[node].parent
		center_col = positions[parent].column
		center_col = center_col + 2

		while current_col < start_col do
			buffer = buffer .. " "
			current_col = current_col + 1
		end
		buffer = buffer .. LEFT_END

		current_col = current_col + 1
		while current_col < center_col do
			buffer = buffer .. LINE
			current_col = current_col + 1
		end
		buffer = buffer .. CENTER

		current_col = current_col + 1
		while current_col < end_col do
			buffer = buffer .. LINE
			current_col = current_col + 1
		end
		buffer = buffer .. RIGHT_END

		current_col = current_col + 1
		node = positions[node].next_member
	end

	print(buffer)
end

function Tree:PrintWeights(row)
	local current_col
	local print_col
	local node
	local print_size
	local next_col
	local final_buffer = ""

	current_col = 0
	node = rows[row].first_member
	while node ~= -1 do
		local buffer = ""

		print_col = positions[node].column + 1
		buffer = string.format("%u", self.nodes[node].weight)

		if #(buffer) < 3 then
			buffer = string.format("%03u", self.nodes[node].weight)
		end

		print_size = 3
		if #(buffer) > 3 then
			if positions[node].next_member == -1 then
				print_size = #(buffer)
			else
				next_col = positions[positions[node].next_member].column
				if next_col - print_col > 6 then
					print_size = #(buffer)
				else
					buffer = "---"
					print_size = 3
				end
			end
		end

		while current_col < print_col do
			buffer = " " .. buffer
			current_col = current_col + 1
		end

		final_buffer = final_buffer .. buffer

		current_col = current_col + print_size
		node = positions[node].next_member
	end
	print(final_buffer)
end

-- Printing the symbol values is a little more complicated.  If it is a
-- printable symbol, I print it between simple quote characters.  If
-- it isn't printable, I print a hex value, which also only takes up three
-- characters.  If it is an internal node, it doesn't have a symbol,
-- which means I just print the vertical line.  There is one complication
-- in this routine.  In order to save space, I check first to see if
-- any of the nodes in this row have a symbol.  If none of them have
-- symbols, we just skip this part, since we don't have to print the
-- row at all.
function Tree:PrintSymbol(row)
	local current_col
	local print_col
	local node
	local buffer = ""

	current_col = 0
	node = rows[row].first_member
	while node ~= -1 do
		if self.nodes[node].child_is_leaf == TRUE then
			break
		end
		node = positions[node].next_member
	end

	if node == -1 then
		return
	end

	node = rows[row].first_member
	while node ~= -1 do
		print_col = positions[node].column + 1
		while current_col < print_col do
			buffer = buffer .. " "
			current_col = current_col + 1
		end

		if self.nodes[node].child_is_leaf == TRUE then
			if IsCharacterPrintable(self.nodes[node].child) == TRUE then
				buffer = buffer .. string.format("'%c'", self.nodes[node].child)
			elseif self.nodes[node].child == END_OF_STREAM then
				buffer = buffer .. "EOF"
			elseif self.nodes[node].child == ESCAPE then
				buffer = buffer .. "ESC"
			else
				buffer = buffer .. string.format("%02XH", self.nodes[node].child)
			end
		else
			buffer = buffer .. string.format(" %s ", VERTICAL)
		end

		current_col = current_col + 3
		node = positions[node].next_member
	end
	print(buffer)
end

function PrintNodeNumbers(row)
	local current_col
	local node
	local print_col
	local buffer = ""

	current_col = 0
	node = rows[row].first_member
	while node ~= -1 do
		print_col = positions[node].column + 1
		while current_col < print_col do
			buffer = buffer .. " "
			current_col = current_col + 1
		end
		buffer = buffer .. string.format("%03d", node)
		current_col = current_col + 3
		node = positions[node].next_member
	end
	print(buffer)
end

function main(arg)
	local inp = assert(io.open(arg[1], "rb"))
	local data = inp:read("*all")
	inp:close()

	local data_stream = DataStream:New()
	for i = 1, #(data) do
		data_stream.byte_arr[i] = data:byte(i)
	end

	local tree = Tree:New()
	tree:Initialize()

	local s = ""
	local c = tree:DecodeSymbol(data_stream)
	while data_stream.number_of_processed_bits <= 8 * #(data_stream.byte_arr) do
		s = s .. string.char(c)
		tree:UpdateModel(c)
		-- tree:PrintTree()
		c = tree:DecodeSymbol(data_stream)
	end

	print(s)
	tree:PrintTree()
end

main(arg)
