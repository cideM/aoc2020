#!/usr/bin/env nix-shell
--[[
#! nix-shell -p "lua53Packages.lua" -i lua
]]

local lines = {[1] = {}}
for line in io.lines() do
	if line:match("^$") then
		lines[#lines + 1] = {}
	end

	for k, v in line:gmatch("(%g+):(%g+)") do
		lines[#lines][k] = v
	end
end

local valid = 0
for _,line in ipairs(lines) do
	if line.byr
		and line.iyr
		and line.eyr
		and line.hgt
		and line.hcl
		and line.ecl
		and line.pid
		then valid = valid + 1
	end
end

local valid2 = 0
for _,line in ipairs(lines) do
	for k,v in pairs(line) do
		if not line.byr
			or not line.iyr
			or not line.eyr
			or not line.hgt
			or not line.hcl
			or not line.ecl
			or not line.pid
			then goto skip
		end

		if k == "byr" 
			and (not (tonumber(v) >= 1920) or not (tonumber(v) <= 2002))
			then goto skip 
		end

		if k == "iyr" 
			and (not (tonumber(v) >= 2010) or not (tonumber(v) <= 2020))
			then goto skip 
		end

		if k == "eyr" 
			and (not (tonumber(v) >= 2020) or not (tonumber(v) <= 2030))
			then goto skip 
		end

		if k == "hgt" then
			if not v:match("(%d+)cm") and not v:match("(%d+)in") then goto skip end
			local h = v:match("(%d+)cm")
			if h 
				and (not (tonumber(h) >=150) or not (tonumber(h) <= 193))
				then goto skip
			end

			local h = v:match("(%d+)in")
			if h 
				and (not (tonumber(h) >= 59) or not (tonumber(h) <= 76))
				then goto skip
			end
		end

		if k == "hcl" then
		        local d = v:match("#([0-9a-f]+)") 
			if not d or #d ~= 6 then goto skip end
		end

		if k == "ecl" 
			and not v:match("amb") 
			and not v:match("blu")
			and not v:match("brn")
			and not v:match("gry")
			and not v:match("grn")
			and not v:match("hzl")
			and not v:match("oth")
			then goto skip 
		end

		if k == "pid" then
			if #v:match("%d+") ~= 9 then goto skip end
		end
	end

	valid2 = valid2 + 1
	::skip::
end

print("Part 1:", valid)
print("Part 2:", valid2)
