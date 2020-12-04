#!/usr/bin/env nix-shell
--[[
#! nix-shell -p "lua53Packages.lua" -i lua
]]

local lines = {[1] = {}}
for line in io.lines() do
	if line:match("^$") then lines[#lines + 1] = {} end
	for k, v in line:gmatch("(%g+):(%g+)") do lines[#lines][k] = v end
end

local required = { byr = true, iyr = true, eyr = true, hgt = true, hcl = true, ecl = true, pid = true }
local valid = 0
for _,l in ipairs(lines) do
	for k, _ in pairs(required) do if not l[k] then goto skip end end
	valid = valid + 1
	::skip::
end

local rules = {
	byr = function (v) return tonumber(v) >= 1920 and tonumber(v) <= 2002 end,
	iyr = function (v) return tonumber(v) >= 2010 and tonumber(v) <= 2020 end,
	eyr = function (v) return tonumber(v) >= 2020 and tonumber(v) <= 2030 end,
	hcl = function (v) local h = v:match("#([0-9a-f]+)") return h and #h == 6 end,
	cid = function (v) return true end,
	pid = function (v) local h = v:match("%d+") return h and #h == 9 end,
	ecl = function (v) return v:match("amb") 
		or v:match("blu")
		or v:match("brn")
		or v:match("gry")
		or v:match("grn")
		or v:match("hzl")
		or v:match("oth")
	end,
	hgt = function (v)
		if not v:match("(%d+)cm") and not v:match("(%d+)in") then return false end
		local h = v:match("(%d+)cm")
		if h then return (tonumber(h) >=150) and (tonumber(h) <= 193) end

		local h = v:match("(%d+)in")
		if h then return (tonumber(h) >=59) and (tonumber(h) <= 76) end
	end,
}

local valid2 = 0
for _,l in ipairs(lines) do
	for k,v in pairs(l) do
		for r, _ in pairs(required) do if not l[r] then goto skip end end
		if not rules[k](v) then goto skip end
	end
	valid2 = valid2 + 1
	::skip::
end

print("Part 1:", valid)
print("Part 2:", valid2)
