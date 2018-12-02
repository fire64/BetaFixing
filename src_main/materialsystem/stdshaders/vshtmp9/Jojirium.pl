push @output, "vs.1.1																								; LINEINFO(Jojirium.vsh)(1)\n";
push @output, ";------------------------------------------------------------------------------																								; LINEINFO(Jojirium.vsh)(3)\n";
push @output, "; Shader specific constant:																								; LINEINFO(Jojirium.vsh)(4)\n";
 	 push @output, ";	 c94, c95	= normal map transform																								; LINEINFO(Jojirium.vsh)(5)\n";
push @output, ";------------------------------------------------------------------------------																								; LINEINFO(Jojirium.vsh)(6)\n";
push @output, ";------------------------------------																								; LINEINFO(macros.vsh)(1)\n";
push @output, "; RULES FOR AUTHORING VERTEX SHADERS:																								; LINEINFO(macros.vsh)(2)\n";
push @output, ";------------------------------------																								; LINEINFO(macros.vsh)(3)\n";
push @output, "; - never use \"def\" . . .set constants in code instead. . our constant shadowing will break otherwise.																								; LINEINFO(macros.vsh)(4)\n";
 	push @output, ";	(same goes for pixel shaders)																								; LINEINFO(macros.vsh)(5)\n";
push @output, "; - use cN notation instead of c[N] notation. .makes grepping for registers easier.																								; LINEINFO(macros.vsh)(6)\n";
    push @output, ";   The only exception is c[a0.x+blah] where you have no choice.																								; LINEINFO(macros.vsh)(7)\n";
$g_NumRegisters = 12;																								
# NOTE: These must match the same values in vsh_prep.pl!																								
$vPos				= "v0";																								
$vBoneWeights		= "v1";																								
$vBoneIndices		= "v2";																								
$vNormal			= "v3";																								
$vColor				= "v5";																								
$vSpecular			= "v6";																								
$vTexCoord0			= "v7";																								
$vTexCoord1			= "v8";																								
$vTexCoord2			= "v9";																								
$vTexCoord3			= "v10";																								
$vTangentS			= "v11";																								
$vTangentT			= "v12";																								
$vUserData			= "v14";																								
if( $g_dx9 )																								
{																								
	if( $g_usesPos )																								
	{																								
		push @output, "		dcl_position $vPos;																								; LINEINFO(macros.vsh)(29)\n";
	}																								
	if( $g_usesBoneWeights )																								
	{																								
		push @output, "		dcl_blendweight $vBoneWeights;																								; LINEINFO(macros.vsh)(33)\n";
	}																								
	if( $g_usesBoneIndices )																								
	{																								
		push @output, "		dcl_blendindices $vBoneIndices;																								; LINEINFO(macros.vsh)(37)\n";
	}																								
	if( $g_usesNormal )																								
	{																								
		push @output, "		dcl_normal $vNormal;																								; LINEINFO(macros.vsh)(41)\n";
	}																								
	if( $g_usesColor )																								
	{																								
		push @output, "		dcl_color0 $vColor;																								; LINEINFO(macros.vsh)(45)\n";
	}																								
	if( $g_usesSpecular )																								
	{																								
		push @output, "		dcl_color1 $vSpecular;																								; LINEINFO(macros.vsh)(49)\n";
	}																								
	if( $g_usesTexCoord0 )																								
	{																								
		push @output, "		dcl_texcoord0 $vTexCoord0;																								; LINEINFO(macros.vsh)(53)\n";
	}																								
	if( $g_usesTexCoord1 )																								
	{																								
		push @output, "		dcl_texcoord1 $vTexCoord1;																								; LINEINFO(macros.vsh)(57)\n";
	}																								
	if( $g_usesTexCoord2 )																								
	{																								
		push @output, "		dcl_texcoord2 $vTexCoord2;																								; LINEINFO(macros.vsh)(61)\n";
	}																								
	if( $g_usesTexCoord3 )																								
	{																								
		push @output, "		dcl_texcoord3 $vTexCoord3;																								; LINEINFO(macros.vsh)(65)\n";
	}																								
	if( $g_usesTangentS )																								
	{																								
		push @output, "		dcl_tangent $vTangentS;																								; LINEINFO(macros.vsh)(69)\n";
	}																								
	if( $g_usesTangentT )																								
	{																								
		push @output, "		dcl_binormal0 $vTangentT;																								; LINEINFO(macros.vsh)(73)\n";
	}																								
	if( $g_usesUserData )																								
	{																								
		push @output, "		dcl_tangent $vUserData;																								; LINEINFO(macros.vsh)(77)\n";
	}																								
}																								
$cConstants0		= "c0";																								
$cZero				= "c0.x";																								
$cOne				= "c0.y";																								
$cTwo				= "c0.z";																								
$cHalf				= "c0.w";																								
$cConstants1		= "c1";																								
$cOOGamma			= "c1.x";																								
#$cThree				= "c1.y";  # NOTE NOTE NOTE: This is overbright now!!!!  Don't use $cThree!!!																								
$cOneThird			= "c1.z";																								
$cOverbrightFactor	= "c1.w";																								
$cEyePos			= "c2";																								
$cWaterZ			= "c2.w";																								
$cEyePosWaterZ		= "c2";																								
$cLightIndex		= "c3";																								
$cLight0Offset		= "c3.x"; # 27																								
$cLight1Offset		= "c3.y"; # 32																								
$cColorToIntScale	= "c3.z"; # 3.0f * 255.0f ~= 765.01																								
$cModel0Index		= "c3.w"; # 42																								
# NOTE: These must match the same values in vsh_prep.pl!																								
$cModelViewProj0	= "c4";																								
$cModelViewProj1	= "c5";																								
$cModelViewProj2	= "c6";																								
$cModelViewProj3	= "c7";																								
$cViewProj0			= "c8";																								
$cViewProj1			= "c9";																								
$cViewProj2			= "c10";																								
$cViewProj3			= "c11";																								
# NOTE: These must match the same values in vsh_prep.pl!																								
$cModelView0		= "c12";																								
$cModelView1		= "c13";																								
$cModelView2		= "c14";																								
$cModelView3		= "c15";																								
$cFogParams			= "c16";																								
$cFogEndOverFogRange = "c16.x";																								
$cFogOne			= "c16.y";																								
$cHeightClipZ		= "c16.z";																								
$cOOFogRange		= "c16.w"; # (1/(fogEnd-fogStart))																								
$cViewModel0		= "c17";																								
$cViewModel1		= "c18";																								
$cViewModel2		= "c19";																								
$cViewModel3		= "c20";																								
$cAmbientColorPosX	= "c21";																								
$cAmbientColorNegX	= "c22";																								
$cAmbientColorPosY	= "c23";																								
$cAmbientColorNegY	= "c24";																								
$cAmbientColorPosZ	= "c25";																								
$cAmbientColorNegZ	= "c26";																								
$cAmbientColorPosXOffset	= "21";																								
$cAmbientColorPosYOffset	= "23";																								
$cAmbientColorPosZOffset	= "25";																								
$cLight0DiffColor	= "c27";																								
$cLight0Dir			= "c28";																								
$cLight0Pos			= "c29";																								
$cLight0SpotParams  = "c30"; # [ exponent, stopdot, stopdot2, 1 / (stopdot - stopdot2)																								
$cLight0Atten		= "c31"; # [ constant, linear, quadratic, 0.0f ]																								
$cLight1DiffColor	= "c32";																								
$cLight1Dir			= "c33";																								
$cLight1Pos			= "c34";																								
$cLight1SpotParams  = "c35"; # [ exponent, stopdot, stopdot2, 1 / (stopdot - stopdot2)																								
$cLight1Atten		= "c36"; # [ constant, linear, quadratic, 0.0f ]																								
# c37-c41 unused! (would be used for a third light if we had one)																								
$cClipDirection		= "c37.x";																								
$cClipDirectionTimesHeightClipZ	= "c37.y";																								
$cModulationColor	= "c38";																								
$cThree				= "c39.x";																								
# There are 16 model matrices for skinning																								
# NOTE: These must match the same values in vsh_prep.pl!																								
$cModel0			= "c42";																								
$cModel1			= "c43";																								
$cModel2			= "c44";																								
# the last cmodel is c89																								
# c90-c95 are reserved for shader specific constants																								
sub OutputUsedRegisters																								
{																								
	local( $i );																								
	push @output, "	; USED REGISTERS																								; LINEINFO(macros.vsh)(174)\n";
	for( $i = 0; $i < $g_NumRegisters; $i++ )																								
	{																								
		if( $g_allocated[$i] )																								
		{																								
			push @output, "			; $g_allocatedname[$i] = r$i																								; LINEINFO(macros.vsh)(179)\n";
		}																								
	}																								
	push @output, "	;																								; LINEINFO(macros.vsh)(182)\n";
}																								
sub AllocateRegister																								
{																								
	local( *reg ) = shift;																								
	local( $regname ) = shift;																								
	local( $i );																								
	for( $i = 0; $i < $g_NumRegisters; $i++ )																								
	{																								
		if( !$g_allocated[$i] )																								
		{																								
			$g_allocated[$i] = 1;																								
			$g_allocatedname[$i] = $regname;																								
			push @output, "			; AllocateRegister $regname = r$i																								; LINEINFO(macros.vsh)(196)\n";
			$reg = "r$i";																								
			&OutputUsedRegisters();																								
			return;																								
		}																								
	}																								
	push @output, "	; Out of registers allocating $regname!																								; LINEINFO(macros.vsh)(202)\n";
	$reg = "rERROR_OUT_OF_REGISTERS";																								
	&OutputUsedRegisters();																								
}																								
# pass in a reference to a var that contains a register. . ie \$var where var will constain "r1", etc																								
sub FreeRegister																								
{																								
	local( *reg ) = shift;																								
	local( $regname ) = shift;																								
	push @output, "	; FreeRegister $regname = $reg																								; LINEINFO(macros.vsh)(212)\n";
	if( $reg =~ m/rERROR_DEALLOCATED/ )																								
	{																								
		push @output, "		; $regname already deallocated																								; LINEINFO(macros.vsh)(215)\n";
		push @output, "		; $reg = \"rALREADY_DEALLOCATED\";																								; LINEINFO(macros.vsh)(216)\n";
		&OutputUsedRegisters();																								
		return;																								
	}																								
#	if( $regname ne g_allocatedname[$reg] )																								
#	{																								
#		; Error freeing $reg																								
#		mov compileerror, freed unallocated register $regname																								
#	}																								
	if( ( $reg =~ m/r(.*)/ ) )																								
	{																								
		$g_allocated[$1] = 0;																								
	}																								
	$reg = "rERROR_DEALLOCATED";																								
	&OutputUsedRegisters();																								
}																								
sub CheckUnfreedRegisters()																								
{																								
	local( $i );																								
	for( $i = 0; $i < $g_NumRegisters; $i++ )																								
	{																								
		if( $g_allocated[$i] )																								
		{																								
			print "ERROR: r$i allocated to $g_allocatedname[$i] at end of program\n";																								
			$g_allocated[$i] = 0;																								
		}																								
	}																								
}																								
sub Normalize																								
{																								
	local( $r ) = shift;																								
	push @output, "	dp3 $r.w, $r, $r																								; LINEINFO(macros.vsh)(250)\n";
	push @output, "	rsq $r.w, $r.w																								; LINEINFO(macros.vsh)(251)\n";
	push @output, "	mul $r, $r, $r.w																								; LINEINFO(macros.vsh)(252)\n";
}																								
sub Cross																								
{																								
	local( $result ) = shift;																								
	local( $a ) = shift;																								
	local( $b ) = shift;																								
	push @output, "	mul $result.xyz, $a.yzx, $b.zxy																								; LINEINFO(macros.vsh)(261)\n";
	push @output, "	mad $result.xyz, -$b.yzx, $a.zxy, $result																								; LINEINFO(macros.vsh)(262)\n";
}																								
sub RangeFog																								
{																								
	push @output, "	; Can either be viewPos or projPos since z should be the same for both.																								; LINEINFO(macros.vsh)(267)\n";
	local( $viewPos ) = shift;																								
	push @output, "	;------------------------------																								; LINEINFO(macros.vsh)(270)\n";
	push @output, "	; Regular range fog																								; LINEINFO(macros.vsh)(271)\n";
	push @output, "	;------------------------------																								; LINEINFO(macros.vsh)(272)\n";
	push @output, "	; oFog.x = 1.0f = no fog																								; LINEINFO(macros.vsh)(274)\n";
	push @output, "	; oFog.x = 0.0f = full fog																								; LINEINFO(macros.vsh)(275)\n";
	push @output, "	; compute fog factor f = (fog_end - dist)*(1/(fog_end-fog_start))																								; LINEINFO(macros.vsh)(276)\n";
	push @output, "	; this is == to: (fog_end/(fog_end-fog_start) - dist/(fog_end-fog_start)																								; LINEINFO(macros.vsh)(277)\n";
	push @output, "	; which can be expressed with a single mad instruction!																								; LINEINFO(macros.vsh)(278)\n";
	if( $g_dx9 )																								
	{																								
		push @output, "		mad oFog, -$viewPos.z, $cOOFogRange, $cFogEndOverFogRange																								; LINEINFO(macros.vsh)(281)\n";
	}																								
	else																								
	{																								
		push @output, "		mad oFog.x, -$viewPos.z, $cOOFogRange, $cFogEndOverFogRange																								; LINEINFO(macros.vsh)(285)\n";
	}																								
}																								
sub WaterFog																								
{																								
	push @output, "	; oFog.x = 1.0f = no fog																								; LINEINFO(macros.vsh)(291)\n";
	push @output, "	; oFog.x = 0.0f = full fog																								; LINEINFO(macros.vsh)(292)\n";
 	push @output, ";	mov oFog.x, $cOne																								; LINEINFO(macros.vsh)(293)\n";
 	push @output, ";	return;																								; LINEINFO(macros.vsh)(294)\n";
	push @output, "	; only $worldPos.z is used out of worldPos																								; LINEINFO(macros.vsh)(295)\n";
	local( $worldPos ) = shift;																								
	local( $viewPos ) = shift;																								
	push @output, "	; $viewPos.z is the distance from the eye to the vertex																								; LINEINFO(macros.vsh)(299)\n";
	local( $tmp );																								
	&AllocateRegister( \$tmp, "\$tmp" );																								
	push @output, "	; Calculate the ratio of the line of sight integral through the water to the total line																								; LINEINFO(macros.vsh)(302)\n";
	push @output, "	; integral																								; LINEINFO(macros.vsh)(303)\n";
	push @output, "	; These could both be done in a single add if cWaterZ and cEyePos.z were in the same constant																								; LINEINFO(macros.vsh)(304)\n";
 	push @output, ";	add $tmp.x, $cWaterZ, -$worldPos.z																								; LINEINFO(macros.vsh)(305)\n";
 	push @output, ";	add $tmp.y, $cEyePos.z, -$worldPos.z																								; LINEINFO(macros.vsh)(306)\n";
	push @output, "	add $tmp.xy, $cEyePosWaterZ.wz, -$worldPos.z																								; LINEINFO(macros.vsh)(307)\n";
	push @output, "	; $tmp.x is the distance from the water surface to the vert																								; LINEINFO(macros.vsh)(309)\n";
	push @output, "	; $tmp.y is the distance from the eye position to the vert																								; LINEINFO(macros.vsh)(310)\n";
	push @output, "	; if $tmp.x < 0, then set it to 0																								; LINEINFO(macros.vsh)(312)\n";
	push @output, "	; This is the equivalent of moving the vert to the water surface if it's above the water surface																								; LINEINFO(macros.vsh)(313)\n";
	push @output, "	max $tmp.x, $tmp.x, $cZero																								; LINEINFO(macros.vsh)(314)\n";
	push @output, "	; $tmp.w = $tmp.x / $tmp.y																								; LINEINFO(macros.vsh)(316)\n";
	push @output, "	rcp $tmp.z, $tmp.y																								; LINEINFO(macros.vsh)(317)\n";
	push @output, "	mul $tmp.w, $tmp.x, $tmp.z																								; LINEINFO(macros.vsh)(318)\n";
	push @output, "	; If the eye is under water, then always use the whole fog amount																								; LINEINFO(macros.vsh)(320)\n";
	push @output, "	; Duh, if the eye is under water, use regular fog!																								; LINEINFO(macros.vsh)(321)\n";
 	push @output, ";	sge $tmp.z, $tmp.y, $cZero																								; LINEINFO(macros.vsh)(322)\n";
	push @output, "	; $tmp.z = 0 if the eye is underwater, otherwise $tmp.z = 1																								; LINEINFO(macros.vsh)(323)\n";
 	push @output, ";	mul $tmp.w, $tmp.w, $tmp.z																								; LINEINFO(macros.vsh)(324)\n";
 	push @output, ";	add $tmp.z, $cOne, -$tmp.z																								; LINEINFO(macros.vsh)(325)\n";
 	push @output, ";	add $tmp.w, $tmp.w, $tmp.z																								; LINEINFO(macros.vsh)(326)\n";
	push @output, "	mul $tmp.w, $tmp.w, $viewPos.z																								; LINEINFO(macros.vsh)(328)\n";
	push @output, "	; $tmp.w is now the distance that we see through water.																								; LINEINFO(macros.vsh)(329)\n";
	if( $g_dx9 )																								
	{																								
		push @output, "		mad oFog, -$tmp.w, $cOOFogRange, $cFogOne																								; LINEINFO(macros.vsh)(333)\n";
	}																								
	else																								
	{																								
		push @output, "		mad oFog.x, -$tmp.w, $cOOFogRange, $cFogOne																								; LINEINFO(macros.vsh)(337)\n";
	}																								
	&FreeRegister( \$tmp, "\$tmp" );																								
}																								
#------------------------------------------------------------------------------																								
# Main fogging routine																								
#------------------------------------------------------------------------------																								
sub CalcFog																								
{																								
	push @output, "	; CalcFog																								; LINEINFO(macros.vsh)(349)\n";
	local( $worldPos ) = shift;																								
	local( $projPos ) = shift;																								
	if( $g_fogType eq "rangefog" )																								
	{																								
		&RangeFog( $projPos );																								
	}																								
	elsif( $g_fogType eq "heightfog" )																								
	{																								
		&WaterFog( $worldPos, $projPos );																								
	}																								
	else																								
	{																								
		die;																								
	}																									
}																								
sub DoHeightClip																								
{																								
	push @output, "	; DoHeightClip																								; LINEINFO(macros.vsh)(370)\n";
 	push @output, ";	$texReg = $cClipDirection * ( $cHeightClipZ - $worldPos.z )																								; LINEINFO(macros.vsh)(371)\n";
 	push @output, ";	$texReg = $cClipDirection * $cHeightClipZ - $cClipDirection * $worldPos.z																								; LINEINFO(macros.vsh)(372)\n";
 	push @output, ";	$const = $cClipDirection * $cHeightClipZ;																								; LINEINFO(macros.vsh)(373)\n";
 	push @output, ";	$texReg = $const - $cClipDirection * $worldPos.z																								; LINEINFO(macros.vsh)(374)\n";
 	push @output, ";	$texReg = ( - $cClipDirection * $worldPos.z ) + $const																								; LINEINFO(macros.vsh)(375)\n";
	local( $worldPos ) = shift;																								
	local( $texReg ) = shift;																								
	local( $tmp );																								
	# Do a user clip plan using texkill in the case that we don't have																								
	# a detail texture.																								
	# optimize!  Can probably do an arbitrary plane in one or two instructions.																								
	if( 0 )																								
	{																								
		&AllocateRegister( \$tmp, "\$tmp" );																								
		push @output, "		add $tmp, -$worldPos.z, $cHeightClipZ																								; LINEINFO(macros.vsh)(387)\n";
		# This determines which side we are clipping on.																								
		push @output, "		mul $texReg, $tmp, $cClipDirection																								; LINEINFO(macros.vsh)(389)\n";
		&FreeRegister( \$tmp, "\$tmp" );																								
	}																								
	else																								
	{																								
		push @output, "		mad $texReg, -$cClipDirection, $worldPos.z, $cClipDirectionTimesHeightClipZ																									; LINEINFO(macros.vsh)(394)\n";
	}																								
}																								
sub GammaToLinear																								
{																								
	local( $gamma ) = shift;																								
	local( $linear ) = shift;																								
	local( $tmp );																								
	&AllocateRegister( \$tmp, "\$tmp" );																								
	push @output, "	; Is rcp more expensive than just storing 2.2 somewhere and doing a mov?																								; LINEINFO(macros.vsh)(406)\n";
	push @output, "	rcp $gamma.w, $cOOGamma							; $gamma.w = 2.2																								; LINEINFO(macros.vsh)(407)\n";
	push @output, "	lit $linear.z, $gamma.zzzw						; r0.z = linear blue																								; LINEINFO(macros.vsh)(408)\n";
	push @output, "	lit $tmp.z, $gamma.yyyw							; r2.z = linear green																								; LINEINFO(macros.vsh)(409)\n";
	push @output, "	mov $linear.y, $tmp.z							; r0.y = linear green																								; LINEINFO(macros.vsh)(410)\n";
	push @output, "	lit $tmp.z, $gamma.xxxw							; r2.z = linear red																								; LINEINFO(macros.vsh)(411)\n";
	push @output, "	mov $linear.x, $tmp.z							; r0.x = linear red																								; LINEINFO(macros.vsh)(412)\n";
	&FreeRegister( \$tmp, "\$tmp" );																								
}																								
sub LinearToGamma																								
{																								
	local( $linear ) = shift;																								
	local( $gamma ) = shift;																								
	local( $tmp );																								
	&AllocateRegister( \$tmp, "\$tmp" );																								
	push @output, "	mov $linear.w, $cOOGamma						; $linear.w = 1.0/2.2																								; LINEINFO(macros.vsh)(425)\n";
	push @output, "	lit $gamma.z, $linear.zzzw						; r0.z = gamma blue																								; LINEINFO(macros.vsh)(426)\n";
	push @output, "	lit $tmp.z, $linear.yyyw						; r2.z = gamma green																								; LINEINFO(macros.vsh)(427)\n";
	push @output, "	mov $gamma.y, $tmp.z							; r0.y = gamma green																								; LINEINFO(macros.vsh)(428)\n";
	push @output, "	lit $tmp.z, $linear.xxxw						; r2.z = gamma red																								; LINEINFO(macros.vsh)(429)\n";
	push @output, "	mov $gamma.x, $tmp.z							; r0.x = gamma red																								; LINEINFO(macros.vsh)(430)\n";
	&FreeRegister( \$tmp, "\$tmp" );																								
}																								
sub ComputeReflectionVector																								
{																								
	local( $worldPos ) = shift;																								
	local( $worldNormal ) = shift;																								
	local( $reflectionVector ) = shift;																								
	local( $vertToEye ); &AllocateRegister( \$vertToEye, "\$vertToEye" );																								
	local( $tmp ); &AllocateRegister( \$tmp, "\$tmp" );																								
	push @output, "	; compute reflection vector r = 2 * (n dot v) n - v																								; LINEINFO(macros.vsh)(444)\n";
	push @output, "	sub $vertToEye.xyz, $cEyePos.xyz, $worldPos  ; $tmp1 = v = c - p																								; LINEINFO(macros.vsh)(445)\n";
	push @output, "	dp3 $tmp, $worldNormal, $vertToEye			; $tmp = n dot v																								; LINEINFO(macros.vsh)(446)\n";
	push @output, "	mul $tmp.xyz, $tmp.xyz, $worldNormal	; $tmp = (n dot v ) n																								; LINEINFO(macros.vsh)(447)\n";
	push @output, "	mad $reflectionVector.xyz, $tmp, $cTwo, -$vertToEye																								; LINEINFO(macros.vsh)(448)\n";
	&FreeRegister( \$vertToEye, "\$vertToEye" );																								
	&FreeRegister( \$tmp, "\$tmp" );																								
}																								
sub ComputeSphereMapTexCoords																								
{																								
	local( $reflectionVector ) = shift;																								
	local( $sphereMapTexCoords ) = shift;																								
	local( $tmp ); &AllocateRegister( \$tmp, "\$tmp" );																								
	push @output, "	; transform reflection vector into view space																								; LINEINFO(macros.vsh)(461)\n";
	push @output, "	dp3 $tmp.x, $reflectionVector, $cViewModel0																								; LINEINFO(macros.vsh)(462)\n";
	push @output, "	dp3 $tmp.y, $reflectionVector, $cViewModel1																								; LINEINFO(macros.vsh)(463)\n";
	push @output, "	dp3 $tmp.z, $reflectionVector, $cViewModel2																								; LINEINFO(macros.vsh)(464)\n";
	push @output, "	; generate <rx ry rz+1>																								; LINEINFO(macros.vsh)(466)\n";
	push @output, "	add $tmp.z, $tmp.z, $cOne																								; LINEINFO(macros.vsh)(467)\n";
	push @output, "	; find 1 / the length of r2																								; LINEINFO(macros.vsh)(469)\n";
	push @output, "	dp3 $tmp.w, $tmp, $tmp																								; LINEINFO(macros.vsh)(470)\n";
	push @output, "	rsq $tmp.w, $tmp.w																								; LINEINFO(macros.vsh)(471)\n";
	push @output, "	; r1 = r2/|r2| + 1																								; LINEINFO(macros.vsh)(473)\n";
	push @output, "	mad $tmp.xy, $tmp.w, $tmp, $cOne																								; LINEINFO(macros.vsh)(474)\n";
	push @output, "	mul $sphereMapTexCoords.xy, $tmp.xy, $cHalf																								; LINEINFO(macros.vsh)(475)\n";
	&FreeRegister( \$tmp, "\$tmp" );																								
}																								
sub SkinPosition																								
{																								
	local( $numBones ) = shift;																								
	local( $worldPos ) = shift;																								
	if( $numBones == 0 )																								
	{																								
		push @output, "		;																								; LINEINFO(macros.vsh)(486)\n";
		push @output, "		; 0 bone skinning (4 instructions)																								; LINEINFO(macros.vsh)(487)\n";
		push @output, "		;																								; LINEINFO(macros.vsh)(488)\n";
		push @output, "		; Transform position into world space																								; LINEINFO(macros.vsh)(489)\n";
		push @output, "		; position																								; LINEINFO(macros.vsh)(490)\n";
		push @output, "		dp4 $worldPos.x, $vPos, $cModel0																								; LINEINFO(macros.vsh)(491)\n";
		push @output, "		dp4 $worldPos.y, $vPos, $cModel1																								; LINEINFO(macros.vsh)(492)\n";
		push @output, "		dp4 $worldPos.z, $vPos, $cModel2																								; LINEINFO(macros.vsh)(493)\n";
		push @output, "		mov $worldPos.w, $cOne																								; LINEINFO(macros.vsh)(494)\n";
	} 																								
	elsif( $numBones == 1 )																								
	{																								
		push @output, "		;																								; LINEINFO(macros.vsh)(498)\n";
		push @output, "		; 1 bone skinning (6 instructions)																								; LINEINFO(macros.vsh)(499)\n";
		push @output, "		;																								; LINEINFO(macros.vsh)(500)\n";
		local( $boneIndices );																								
		&AllocateRegister( \$boneIndices, "\$boneIndices" );																								
		push @output, "		; Perform 1 bone skinning																								; LINEINFO(macros.vsh)(505)\n";
		push @output, "		; Transform position into world space																								; LINEINFO(macros.vsh)(506)\n";
		push @output, "		; denormalize d3dcolor to matrix index																								; LINEINFO(macros.vsh)(507)\n";
		push @output, "		mad $boneIndices, $vBoneIndices, $cColorToIntScale, $cModel0Index																								; LINEINFO(macros.vsh)(508)\n";
		push @output, "		mov a0.x, $boneIndices.z																								; LINEINFO(macros.vsh)(509)\n";
		push @output, "		; position																								; LINEINFO(macros.vsh)(511)\n";
		push @output, "		dp4 $worldPos.x, $vPos, c[a0.x]																								; LINEINFO(macros.vsh)(512)\n";
		push @output, "		dp4 $worldPos.y, $vPos, c[a0.x + 1]																								; LINEINFO(macros.vsh)(513)\n";
		push @output, "		dp4 $worldPos.z, $vPos, c[a0.x + 2]																								; LINEINFO(macros.vsh)(514)\n";
		push @output, "		mov $worldPos.w, $cOne																								; LINEINFO(macros.vsh)(515)\n";
		&FreeRegister( \$boneIndices, "\$boneIndices" );																								
	}																								
	elsif( $numBones == 2 )																								
	{																								
		push @output, "		;																								; LINEINFO(macros.vsh)(521)\n";
		push @output, "		; 2 bone skinning (13 instructions)																								; LINEINFO(macros.vsh)(522)\n";
		push @output, "		;																								; LINEINFO(macros.vsh)(523)\n";
		local( $boneIndices );																								
		local( $blendedMatrix0 );																								
		local( $blendedMatrix1 );																								
		local( $blendedMatrix2 );																								
		&AllocateRegister( \$boneIndices, "\$boneIndices" );																								
		&AllocateRegister( \$blendedMatrix0, "\$blendedMatrix0" );																								
		&AllocateRegister( \$blendedMatrix1, "\$blendedMatrix1" );																								
		&AllocateRegister( \$blendedMatrix2, "\$blendedMatrix2" );																								
		push @output, "		; Transform position into world space using all bones																								; LINEINFO(macros.vsh)(534)\n";
		push @output, "		; denormalize d3dcolor to matrix index																								; LINEINFO(macros.vsh)(535)\n";
		push @output, "		mad $boneIndices, $vBoneIndices, $cColorToIntScale, $cModel0Index																								; LINEINFO(macros.vsh)(536)\n";
		push @output, "		; r11 = boneindices at this point																								; LINEINFO(macros.vsh)(537)\n";
		push @output, "		; first matrix																								; LINEINFO(macros.vsh)(538)\n";
		push @output, "		mov a0.x, $boneIndices.z																								; LINEINFO(macros.vsh)(539)\n";
		push @output, "		mul $blendedMatrix0, $vBoneWeights.x, c[a0.x]																								; LINEINFO(macros.vsh)(540)\n";
		push @output, "		mul $blendedMatrix1, $vBoneWeights.x, c[a0.x+1]																								; LINEINFO(macros.vsh)(541)\n";
		push @output, "		mul $blendedMatrix2, $vBoneWeights.x, c[a0.x+2]																								; LINEINFO(macros.vsh)(542)\n";
		push @output, "		; second matrix																								; LINEINFO(macros.vsh)(543)\n";
		push @output, "		mov a0.x, $boneIndices.y																								; LINEINFO(macros.vsh)(544)\n";
		push @output, "		mad $blendedMatrix0, $vBoneWeights.y, c[a0.x], $blendedMatrix0																								; LINEINFO(macros.vsh)(545)\n";
		push @output, "		mad $blendedMatrix1, $vBoneWeights.y, c[a0.x+1], $blendedMatrix1																								; LINEINFO(macros.vsh)(546)\n";
		push @output, "		mad $blendedMatrix2, $vBoneWeights.y, c[a0.x+2], $blendedMatrix2																								; LINEINFO(macros.vsh)(547)\n";
		push @output, "		; position																								; LINEINFO(macros.vsh)(549)\n";
		push @output, "		dp4 $worldPos.x, $vPos, $blendedMatrix0																								; LINEINFO(macros.vsh)(550)\n";
		push @output, "		dp4 $worldPos.y, $vPos, $blendedMatrix1																								; LINEINFO(macros.vsh)(551)\n";
		push @output, "		dp4 $worldPos.z, $vPos, $blendedMatrix2																								; LINEINFO(macros.vsh)(552)\n";
		push @output, "		mov $worldPos.w, $cOne																								; LINEINFO(macros.vsh)(553)\n";
		&FreeRegister( \$boneIndices, "\$boneIndices" );																								
		&FreeRegister( \$blendedMatrix0, "\$blendedMatrix0" );																								
		&FreeRegister( \$blendedMatrix1, "\$blendedMatrix1" );																								
		&FreeRegister( \$blendedMatrix2, "\$blendedMatrix2" );																								
	}																								
	elsif( $numBones == 3 )																								
	{																								
		push @output, "		;																								; LINEINFO(macros.vsh)(562)\n";
		push @output, "		; 3 bone skinning  (19 instructions)																								; LINEINFO(macros.vsh)(563)\n";
		push @output, "		;																								; LINEINFO(macros.vsh)(564)\n";
		local( $boneIndices );																								
		local( $blendedMatrix0 );																								
		local( $blendedMatrix1 );																								
		local( $blendedMatrix2 );																								
		&AllocateRegister( \$boneIndices, "\$boneIndices" );																								
		&AllocateRegister( \$blendedMatrix0, "\$blendedMatrix0" );																								
		&AllocateRegister( \$blendedMatrix1, "\$blendedMatrix1" );																								
		&AllocateRegister( \$blendedMatrix2, "\$blendedMatrix2" );																								
		push @output, "		; Transform position into world space using all bones																								; LINEINFO(macros.vsh)(574)\n";
		push @output, "		; denormalize d3dcolor to matrix index																								; LINEINFO(macros.vsh)(575)\n";
		push @output, "		mad $boneIndices, $vBoneIndices, $cColorToIntScale, $cModel0Index																								; LINEINFO(macros.vsh)(576)\n";
		push @output, "		; r11 = boneindices at this point																								; LINEINFO(macros.vsh)(577)\n";
		push @output, "		; first matrix																								; LINEINFO(macros.vsh)(578)\n";
		push @output, "		mov a0.x, $boneIndices.z																								; LINEINFO(macros.vsh)(579)\n";
		push @output, "		mul $blendedMatrix0, $vBoneWeights.x, c[a0.x]																								; LINEINFO(macros.vsh)(580)\n";
		push @output, "		mul $blendedMatrix1, $vBoneWeights.x, c[a0.x+1]																								; LINEINFO(macros.vsh)(581)\n";
		push @output, "		mul $blendedMatrix2, $vBoneWeights.x, c[a0.x+2]																								; LINEINFO(macros.vsh)(582)\n";
		push @output, "		; second matrix																								; LINEINFO(macros.vsh)(583)\n";
		push @output, "		mov a0.x, $boneIndices.y																								; LINEINFO(macros.vsh)(584)\n";
		push @output, "		mad $blendedMatrix0, $vBoneWeights.y, c[a0.x], $blendedMatrix0																								; LINEINFO(macros.vsh)(585)\n";
		push @output, "		mad $blendedMatrix1, $vBoneWeights.y, c[a0.x+1], $blendedMatrix1																								; LINEINFO(macros.vsh)(586)\n";
		push @output, "		mad $blendedMatrix2, $vBoneWeights.y, c[a0.x+2], $blendedMatrix2																								; LINEINFO(macros.vsh)(587)\n";
		push @output, "		; Calculate third weight																								; LINEINFO(macros.vsh)(589)\n";
		push @output, "		; compute 1-(weight1+weight2) to calculate weight2																								; LINEINFO(macros.vsh)(590)\n";
		push @output, "		; Use $boneIndices.w as a temp since we aren't using it for anything.																								; LINEINFO(macros.vsh)(591)\n";
		push @output, "		add $boneIndices.w, $vBoneWeights.x, $vBoneWeights.y																								; LINEINFO(macros.vsh)(592)\n";
		push @output, "		sub $boneIndices.w, $cOne, $boneIndices.w																								; LINEINFO(macros.vsh)(593)\n";
		push @output, "		; third matrix																								; LINEINFO(macros.vsh)(595)\n";
		push @output, "		mov a0.x, $boneIndices.x																								; LINEINFO(macros.vsh)(596)\n";
		push @output, "		mad $blendedMatrix0, $boneIndices.w, c[a0.x], $blendedMatrix0																								; LINEINFO(macros.vsh)(597)\n";
		push @output, "		mad $blendedMatrix1, $boneIndices.w, c[a0.x+1], $blendedMatrix1																								; LINEINFO(macros.vsh)(598)\n";
		push @output, "		mad $blendedMatrix2, $boneIndices.w, c[a0.x+2], $blendedMatrix2																								; LINEINFO(macros.vsh)(599)\n";
		push @output, "		; position																								; LINEINFO(macros.vsh)(601)\n";
		push @output, "		dp4 $worldPos.x, $vPos, $blendedMatrix0																								; LINEINFO(macros.vsh)(602)\n";
		push @output, "		dp4 $worldPos.y, $vPos, $blendedMatrix1																								; LINEINFO(macros.vsh)(603)\n";
		push @output, "		dp4 $worldPos.z, $vPos, $blendedMatrix2																								; LINEINFO(macros.vsh)(604)\n";
		push @output, "		mov $worldPos.w, $cOne																								; LINEINFO(macros.vsh)(605)\n";
		&FreeRegister( \$boneIndices, "\$boneIndices" );																								
		&FreeRegister( \$blendedMatrix0, "\$blendedMatrix0" );																								
		&FreeRegister( \$blendedMatrix1, "\$blendedMatrix1" );																								
		&FreeRegister( \$blendedMatrix2, "\$blendedMatrix2" );																								
	}																								
}																								
sub SkinPositionAndNormal																								
{																								
	local( $numBones ) = shift;																								
	local( $worldPos ) = shift;																								
	local( $worldNormal ) = shift;																								
	if( $numBones == 0 )																								
	{																								
		push @output, "		;																								; LINEINFO(macros.vsh)(622)\n";
		push @output, "		; 0 bone skinning (13 instructions)																								; LINEINFO(macros.vsh)(623)\n";
		push @output, "		;																								; LINEINFO(macros.vsh)(624)\n";
		push @output, "		; Transform position + normal + tangentS + tangentT into world space																								; LINEINFO(macros.vsh)(625)\n";
		push @output, "		; position																								; LINEINFO(macros.vsh)(626)\n";
		push @output, "		dp4 $worldPos.x, $vPos, $cModel0																								; LINEINFO(macros.vsh)(627)\n";
		push @output, "		dp4 $worldPos.y, $vPos, $cModel1																								; LINEINFO(macros.vsh)(628)\n";
		push @output, "		dp4 $worldPos.z, $vPos, $cModel2																								; LINEINFO(macros.vsh)(629)\n";
		push @output, "		mov $worldPos.w, $cOne																								; LINEINFO(macros.vsh)(630)\n";
		push @output, "		; normal																								; LINEINFO(macros.vsh)(631)\n";
		push @output, "		dp3 $worldNormal.x, $vNormal, $cModel0																								; LINEINFO(macros.vsh)(632)\n";
		push @output, "		dp3 $worldNormal.y, $vNormal, $cModel1																								; LINEINFO(macros.vsh)(633)\n";
		push @output, "		dp3 $worldNormal.z, $vNormal, $cModel2																								; LINEINFO(macros.vsh)(634)\n";
	}																								
	elsif( $numBones == 1 )																								
	{																								
		push @output, "		;																								; LINEINFO(macros.vsh)(638)\n";
		push @output, "		; 1 bone skinning (17 instructions)																								; LINEINFO(macros.vsh)(639)\n";
		push @output, "		;																								; LINEINFO(macros.vsh)(640)\n";
		local( $boneIndices );																								
		&AllocateRegister( \$boneIndices, "\$boneIndices" );																								
		push @output, "		; Perform 1 bone skinning																								; LINEINFO(macros.vsh)(645)\n";
		push @output, "		; Transform position into world space																								; LINEINFO(macros.vsh)(646)\n";
		push @output, "		; denormalize d3dcolor to matrix index																								; LINEINFO(macros.vsh)(647)\n";
		push @output, "		mad $boneIndices, $vBoneIndices, $cColorToIntScale, $cModel0Index																								; LINEINFO(macros.vsh)(648)\n";
		push @output, "		mov a0.x, $boneIndices.z																								; LINEINFO(macros.vsh)(649)\n";
		push @output, "		; position																								; LINEINFO(macros.vsh)(651)\n";
		push @output, "		dp4 $worldPos.x, $vPos, c[a0.x]																								; LINEINFO(macros.vsh)(652)\n";
		push @output, "		dp4 $worldPos.y, $vPos, c[a0.x + 1]																								; LINEINFO(macros.vsh)(653)\n";
		push @output, "		dp4 $worldPos.z, $vPos, c[a0.x + 2]																								; LINEINFO(macros.vsh)(654)\n";
		push @output, "		mov $worldPos.w, $cOne																								; LINEINFO(macros.vsh)(655)\n";
		push @output, "		; normal																								; LINEINFO(macros.vsh)(657)\n";
		push @output, "		dp3 $worldNormal.x, $vNormal, c[a0.x]																								; LINEINFO(macros.vsh)(658)\n";
		push @output, "		dp3 $worldNormal.y, $vNormal, c[a0.x + 1]																								; LINEINFO(macros.vsh)(659)\n";
		push @output, "		dp3 $worldNormal.z, $vNormal, c[a0.x + 2]																								; LINEINFO(macros.vsh)(660)\n";
		&FreeRegister( \$boneIndices, "\$boneIndices" );																								
	}																								
	elsif( $numBones == 2 )																								
	{																								
		push @output, "		;																								; LINEINFO(macros.vsh)(666)\n";
		push @output, "		; 2 bone skinning (16 instructions)																								; LINEINFO(macros.vsh)(667)\n";
		push @output, "		;																								; LINEINFO(macros.vsh)(668)\n";
		local( $boneIndices );																								
		local( $blendedMatrix0 );																								
		local( $blendedMatrix1 );																								
		local( $blendedMatrix2 );																								
		&AllocateRegister( \$boneIndices, "\$boneIndices" );																								
		&AllocateRegister( \$blendedMatrix0, "\$blendedMatrix0" );																								
		&AllocateRegister( \$blendedMatrix1, "\$blendedMatrix1" );																								
		&AllocateRegister( \$blendedMatrix2, "\$blendedMatrix2" );																								
		push @output, "		; Transform position into world space using all bones																								; LINEINFO(macros.vsh)(679)\n";
		push @output, "		; denormalize d3dcolor to matrix index																								; LINEINFO(macros.vsh)(680)\n";
		push @output, "		mad $boneIndices, $vBoneIndices, $cColorToIntScale, $cModel0Index																								; LINEINFO(macros.vsh)(681)\n";
		push @output, "		; r11 = boneindices at this point																								; LINEINFO(macros.vsh)(682)\n";
		push @output, "		; first matrix																								; LINEINFO(macros.vsh)(683)\n";
		push @output, "		mov a0.x, $boneIndices.z																								; LINEINFO(macros.vsh)(684)\n";
		push @output, "		mul $blendedMatrix0, $vBoneWeights.x, c[a0.x]																								; LINEINFO(macros.vsh)(685)\n";
		push @output, "		mul $blendedMatrix1, $vBoneWeights.x, c[a0.x+1]																								; LINEINFO(macros.vsh)(686)\n";
		push @output, "		mul $blendedMatrix2, $vBoneWeights.x, c[a0.x+2]																								; LINEINFO(macros.vsh)(687)\n";
		push @output, "		; second matrix																								; LINEINFO(macros.vsh)(688)\n";
		push @output, "		mov a0.x, $boneIndices.y																								; LINEINFO(macros.vsh)(689)\n";
		push @output, "		mad $blendedMatrix0, $vBoneWeights.y, c[a0.x], $blendedMatrix0																								; LINEINFO(macros.vsh)(690)\n";
		push @output, "		mad $blendedMatrix1, $vBoneWeights.y, c[a0.x+1], $blendedMatrix1																								; LINEINFO(macros.vsh)(691)\n";
		push @output, "		mad $blendedMatrix2, $vBoneWeights.y, c[a0.x+2], $blendedMatrix2																								; LINEINFO(macros.vsh)(692)\n";
		push @output, "		; position																								; LINEINFO(macros.vsh)(694)\n";
		push @output, "		dp4 $worldPos.x, $vPos, $blendedMatrix0																								; LINEINFO(macros.vsh)(695)\n";
		push @output, "		dp4 $worldPos.y, $vPos, $blendedMatrix1																								; LINEINFO(macros.vsh)(696)\n";
		push @output, "		dp4 $worldPos.z, $vPos, $blendedMatrix2																								; LINEINFO(macros.vsh)(697)\n";
		push @output, "		mov $worldPos.w, $cOne																								; LINEINFO(macros.vsh)(698)\n";
		push @output, "		; normal																								; LINEINFO(macros.vsh)(700)\n";
		push @output, "		dp3 $worldNormal.x, $vNormal, $blendedMatrix0																								; LINEINFO(macros.vsh)(701)\n";
		push @output, "		dp3 $worldNormal.y, $vNormal, $blendedMatrix1																								; LINEINFO(macros.vsh)(702)\n";
		push @output, "		dp3 $worldNormal.z, $vNormal, $blendedMatrix2																								; LINEINFO(macros.vsh)(703)\n";
		&FreeRegister( \$boneIndices, "\$boneIndices" );																								
		&FreeRegister( \$blendedMatrix0, "\$blendedMatrix0" );																								
		&FreeRegister( \$blendedMatrix1, "\$blendedMatrix1" );																								
		&FreeRegister( \$blendedMatrix2, "\$blendedMatrix2" );																								
	}																								
	elsif( $numBones == 3 )																								
	{																								
		local( $boneIndices );																								
		local( $blendedMatrix0 );																								
		local( $blendedMatrix1 );																								
		local( $blendedMatrix2 );																								
		&AllocateRegister( \$boneIndices, "\$boneIndices" );																								
		&AllocateRegister( \$blendedMatrix0, "\$blendedMatrix0" );																								
		&AllocateRegister( \$blendedMatrix1, "\$blendedMatrix1" );																								
		&AllocateRegister( \$blendedMatrix2, "\$blendedMatrix2" );																								
		push @output, "		; Transform position into world space using all bones																								; LINEINFO(macros.vsh)(721)\n";
		push @output, "		; denormalize d3dcolor to matrix index																								; LINEINFO(macros.vsh)(722)\n";
		push @output, "		mad $boneIndices, $vBoneIndices, $cColorToIntScale, $cModel0Index																								; LINEINFO(macros.vsh)(723)\n";
		push @output, "		; r11 = boneindices at this point																								; LINEINFO(macros.vsh)(724)\n";
		push @output, "		; first matrix																								; LINEINFO(macros.vsh)(725)\n";
		push @output, "		mov a0.x, $boneIndices.z																								; LINEINFO(macros.vsh)(726)\n";
		push @output, "		mul $blendedMatrix0, $vBoneWeights.x, c[a0.x]																								; LINEINFO(macros.vsh)(727)\n";
		push @output, "		mul $blendedMatrix1, $vBoneWeights.x, c[a0.x+1]																								; LINEINFO(macros.vsh)(728)\n";
		push @output, "		mul $blendedMatrix2, $vBoneWeights.x, c[a0.x+2]																								; LINEINFO(macros.vsh)(729)\n";
		push @output, "		; second matrix																								; LINEINFO(macros.vsh)(730)\n";
		push @output, "		mov a0.x, $boneIndices.y																								; LINEINFO(macros.vsh)(731)\n";
		push @output, "		mad $blendedMatrix0, $vBoneWeights.y, c[a0.x], $blendedMatrix0																								; LINEINFO(macros.vsh)(732)\n";
		push @output, "		mad $blendedMatrix1, $vBoneWeights.y, c[a0.x+1], $blendedMatrix1																								; LINEINFO(macros.vsh)(733)\n";
		push @output, "		mad $blendedMatrix2, $vBoneWeights.y, c[a0.x+2], $blendedMatrix2																								; LINEINFO(macros.vsh)(734)\n";
		push @output, "		; Calculate third weight																								; LINEINFO(macros.vsh)(736)\n";
		push @output, "		; compute 1-(weight1+weight2) to calculate weight2																								; LINEINFO(macros.vsh)(737)\n";
		push @output, "		; Use $boneIndices.w as a temp since we aren't using it for anything.																								; LINEINFO(macros.vsh)(738)\n";
		push @output, "		add $boneIndices.w, $vBoneWeights.x, $vBoneWeights.y																								; LINEINFO(macros.vsh)(739)\n";
		push @output, "		sub $boneIndices.w, $cOne, $boneIndices.w																								; LINEINFO(macros.vsh)(740)\n";
		push @output, "		; third matrix																								; LINEINFO(macros.vsh)(742)\n";
		push @output, "		mov a0.x, $boneIndices.x																								; LINEINFO(macros.vsh)(743)\n";
		push @output, "		mad $blendedMatrix0, $boneIndices.w, c[a0.x], $blendedMatrix0																								; LINEINFO(macros.vsh)(744)\n";
		push @output, "		mad $blendedMatrix1, $boneIndices.w, c[a0.x+1], $blendedMatrix1																								; LINEINFO(macros.vsh)(745)\n";
		push @output, "		mad $blendedMatrix2, $boneIndices.w, c[a0.x+2], $blendedMatrix2																								; LINEINFO(macros.vsh)(746)\n";
		push @output, "		; position																								; LINEINFO(macros.vsh)(748)\n";
		push @output, "		dp4 $worldPos.x, $vPos, $blendedMatrix0																								; LINEINFO(macros.vsh)(749)\n";
		push @output, "		dp4 $worldPos.y, $vPos, $blendedMatrix1																								; LINEINFO(macros.vsh)(750)\n";
		push @output, "		dp4 $worldPos.z, $vPos, $blendedMatrix2																								; LINEINFO(macros.vsh)(751)\n";
		push @output, "		mov $worldPos.w, $cOne																								; LINEINFO(macros.vsh)(752)\n";
		push @output, "		; normal																								; LINEINFO(macros.vsh)(754)\n";
		push @output, "		dp3 $worldNormal.x, $vNormal, $blendedMatrix0																								; LINEINFO(macros.vsh)(755)\n";
		push @output, "		dp3 $worldNormal.y, $vNormal, $blendedMatrix1																								; LINEINFO(macros.vsh)(756)\n";
		push @output, "		dp3 $worldNormal.z, $vNormal, $blendedMatrix2																								; LINEINFO(macros.vsh)(757)\n";
		&FreeRegister( \$boneIndices, "\$boneIndices" );																								
		&FreeRegister( \$blendedMatrix0, "\$blendedMatrix0" );																								
		&FreeRegister( \$blendedMatrix1, "\$blendedMatrix1" );																								
		&FreeRegister( \$blendedMatrix2, "\$blendedMatrix2" );																								
	}																									
}																								
sub SkinPositionNormalAndTangentSpace																								
{																								
	local( $numBones ) = shift;																								
	local( $worldPos ) = shift;																								
	local( $worldNormal ) = shift;																								
	local( $worldTangentS ) = shift;																								
	local( $worldTangentT ) = shift;																								
	if( $numBones == 0 )																								
	{																								
		push @output, "		;																								; LINEINFO(macros.vsh)(775)\n";
		push @output, "		; 0 bone skinning (13 instructions)																								; LINEINFO(macros.vsh)(776)\n";
		push @output, "		;																								; LINEINFO(macros.vsh)(777)\n";
		push @output, "		; Transform position + normal + tangentS + tangentT into world space																								; LINEINFO(macros.vsh)(778)\n";
		push @output, "		; position																								; LINEINFO(macros.vsh)(780)\n";
		push @output, "		dp4 $worldPos.x, $vPos, $cModel0																								; LINEINFO(macros.vsh)(781)\n";
		push @output, "		dp4 $worldPos.y, $vPos, $cModel1																								; LINEINFO(macros.vsh)(782)\n";
		push @output, "		dp4 $worldPos.z, $vPos, $cModel2																								; LINEINFO(macros.vsh)(783)\n";
		push @output, "		mov $worldPos.w, $cOne																								; LINEINFO(macros.vsh)(784)\n";
		push @output, "		; normal																								; LINEINFO(macros.vsh)(786)\n";
		push @output, "		dp3 $worldNormal.x, $vNormal, $cModel0																								; LINEINFO(macros.vsh)(787)\n";
		push @output, "		dp3 $worldNormal.y, $vNormal, $cModel1																								; LINEINFO(macros.vsh)(788)\n";
		push @output, "		dp3 $worldNormal.z, $vNormal, $cModel2																								; LINEINFO(macros.vsh)(789)\n";
		push @output, "		; tangents																								; LINEINFO(macros.vsh)(791)\n";
		push @output, "		dp3 $worldTangentS.x, $vUserData, $cModel0																								; LINEINFO(macros.vsh)(792)\n";
		push @output, "		dp3 $worldTangentS.y, $vUserData, $cModel1																								; LINEINFO(macros.vsh)(793)\n";
		push @output, "		dp3 $worldTangentS.z, $vUserData, $cModel2																								; LINEINFO(macros.vsh)(794)\n";
		push @output, "		; calculate tangent t via cross( N, S ) * S[3]																								; LINEINFO(macros.vsh)(796)\n";
		&Cross( $worldTangentT, $worldNormal, $worldTangentS );																								
		push @output, "		mul $worldTangentT.xyz, $vUserData.w, $worldTangentT.xyz																								; LINEINFO(macros.vsh)(798)\n";
	}																								
	elsif( $numBones == 1 )																								
	{																								
		push @output, "		;																								; LINEINFO(macros.vsh)(802)\n";
		push @output, "		; 1 bone skinning (17 instructions)																								; LINEINFO(macros.vsh)(803)\n";
		push @output, "		;																								; LINEINFO(macros.vsh)(804)\n";
		local( $boneIndices );																								
		&AllocateRegister( \$boneIndices, "\$boneIndices" );																								
		push @output, "		; Perform 1 bone skinning																								; LINEINFO(macros.vsh)(809)\n";
		push @output, "		; Transform position into world space																								; LINEINFO(macros.vsh)(810)\n";
		push @output, "		; denormalize d3dcolor to matrix index																								; LINEINFO(macros.vsh)(811)\n";
		push @output, "		mad $boneIndices, $vBoneIndices, $cColorToIntScale, $cModel0Index																								; LINEINFO(macros.vsh)(812)\n";
		push @output, "		mov a0.x, $boneIndices.z																								; LINEINFO(macros.vsh)(813)\n";
		push @output, "		; position																								; LINEINFO(macros.vsh)(815)\n";
		push @output, "		dp4 $worldPos.x, $vPos, c[a0.x]																								; LINEINFO(macros.vsh)(816)\n";
		push @output, "		dp4 $worldPos.y, $vPos, c[a0.x + 1]																								; LINEINFO(macros.vsh)(817)\n";
		push @output, "		dp4 $worldPos.z, $vPos, c[a0.x + 2]																								; LINEINFO(macros.vsh)(818)\n";
		push @output, "		mov $worldPos.w, $cOne																								; LINEINFO(macros.vsh)(819)\n";
		push @output, "		; normal																								; LINEINFO(macros.vsh)(821)\n";
		push @output, "		dp3 $worldNormal.x, $vNormal, c[a0.x]																								; LINEINFO(macros.vsh)(822)\n";
		push @output, "		dp3 $worldNormal.y, $vNormal, c[a0.x + 1]																								; LINEINFO(macros.vsh)(823)\n";
		push @output, "		dp3 $worldNormal.z, $vNormal, c[a0.x + 2]																								; LINEINFO(macros.vsh)(824)\n";
		push @output, "		; tangents																								; LINEINFO(macros.vsh)(826)\n";
		push @output, "		dp3 $worldTangentS.x, $vUserData, c[a0.x]																								; LINEINFO(macros.vsh)(827)\n";
		push @output, "		dp3 $worldTangentS.y, $vUserData, c[a0.x + 1]																								; LINEINFO(macros.vsh)(828)\n";
		push @output, "		dp3 $worldTangentS.z, $vUserData, c[a0.x + 2]																								; LINEINFO(macros.vsh)(829)\n";
		push @output, "		; calculate tangent t via cross( N, S ) * S[3]																								; LINEINFO(macros.vsh)(831)\n";
		&Cross( $worldTangentT, $worldNormal, $worldTangentS );																								
		push @output, "		mul $worldTangentT.xyz, $vUserData.w, $worldTangentT.xyz																								; LINEINFO(macros.vsh)(833)\n";
		&FreeRegister( \$boneIndices, "\$boneIndices" );																								
	}																								
	elsif( $numBones == 2 )																								
	{																								
		push @output, "		;																								; LINEINFO(macros.vsh)(839)\n";
		push @output, "		; 2 bone skinning (22 instructions)																								; LINEINFO(macros.vsh)(840)\n";
		push @output, "		;																								; LINEINFO(macros.vsh)(841)\n";
		local( $boneIndices );																								
		local( $blendedMatrix0 );																								
		local( $blendedMatrix1 );																								
		local( $blendedMatrix2 );																								
		&AllocateRegister( \$boneIndices, "\$boneIndices" );																								
		&AllocateRegister( \$blendedMatrix0, "\$blendedMatrix0" );																								
		&AllocateRegister( \$blendedMatrix1, "\$blendedMatrix1" );																								
		&AllocateRegister( \$blendedMatrix2, "\$blendedMatrix2" );																								
		push @output, "		; Transform position into world space using all bones																								; LINEINFO(macros.vsh)(852)\n";
		push @output, "		; denormalize d3dcolor to matrix index																								; LINEINFO(macros.vsh)(853)\n";
		push @output, "		mad $boneIndices, $vBoneIndices, $cColorToIntScale, $cModel0Index																								; LINEINFO(macros.vsh)(854)\n";
		push @output, "		; r11 = boneindices at this point																								; LINEINFO(macros.vsh)(855)\n";
		push @output, "		; first matrix																								; LINEINFO(macros.vsh)(856)\n";
		push @output, "		mov a0.x, $boneIndices.z																								; LINEINFO(macros.vsh)(857)\n";
		push @output, "		mul $blendedMatrix0, $vBoneWeights.x, c[a0.x]																								; LINEINFO(macros.vsh)(858)\n";
		push @output, "		mul $blendedMatrix1, $vBoneWeights.x, c[a0.x+1]																								; LINEINFO(macros.vsh)(859)\n";
		push @output, "		mul $blendedMatrix2, $vBoneWeights.x, c[a0.x+2]																								; LINEINFO(macros.vsh)(860)\n";
		push @output, "		; second matrix																								; LINEINFO(macros.vsh)(861)\n";
		push @output, "		mov a0.x, $boneIndices.y																								; LINEINFO(macros.vsh)(862)\n";
		push @output, "		mad $blendedMatrix0, $vBoneWeights.y, c[a0.x], $blendedMatrix0																								; LINEINFO(macros.vsh)(863)\n";
		push @output, "		mad $blendedMatrix1, $vBoneWeights.y, c[a0.x+1], $blendedMatrix1																								; LINEINFO(macros.vsh)(864)\n";
		push @output, "		mad $blendedMatrix2, $vBoneWeights.y, c[a0.x+2], $blendedMatrix2																								; LINEINFO(macros.vsh)(865)\n";
		push @output, "		; position																								; LINEINFO(macros.vsh)(867)\n";
		push @output, "		dp4 $worldPos.x, $vPos, $blendedMatrix0																								; LINEINFO(macros.vsh)(868)\n";
		push @output, "		dp4 $worldPos.y, $vPos, $blendedMatrix1																								; LINEINFO(macros.vsh)(869)\n";
		push @output, "		dp4 $worldPos.z, $vPos, $blendedMatrix2																								; LINEINFO(macros.vsh)(870)\n";
		push @output, "		mov $worldPos.w, $cOne																								; LINEINFO(macros.vsh)(871)\n";
		push @output, "		; normal																								; LINEINFO(macros.vsh)(873)\n";
		push @output, "		dp3 $worldNormal.x, $vNormal, $blendedMatrix0																								; LINEINFO(macros.vsh)(874)\n";
		push @output, "		dp3 $worldNormal.y, $vNormal, $blendedMatrix1																								; LINEINFO(macros.vsh)(875)\n";
		push @output, "		dp3 $worldNormal.z, $vNormal, $blendedMatrix2																								; LINEINFO(macros.vsh)(876)\n";
		push @output, "		; tangents																								; LINEINFO(macros.vsh)(878)\n";
		push @output, "		dp3 $worldTangentS.x, $vUserData, $blendedMatrix0																								; LINEINFO(macros.vsh)(879)\n";
		push @output, "		dp3 $worldTangentS.y, $vUserData, $blendedMatrix1																								; LINEINFO(macros.vsh)(880)\n";
		push @output, "		dp3 $worldTangentS.z, $vUserData, $blendedMatrix2																								; LINEINFO(macros.vsh)(881)\n";
		push @output, "		; calculate tangent t via cross( N, S ) * S[3]																								; LINEINFO(macros.vsh)(883)\n";
		&Cross( $worldTangentT, $worldNormal, $worldTangentS );																								
		push @output, "		mul $worldTangentT.xyz, $vUserData.w, $worldTangentT.xyz																								; LINEINFO(macros.vsh)(885)\n";
		&FreeRegister( \$boneIndices, "\$boneIndices" );																								
		&FreeRegister( \$blendedMatrix0, "\$blendedMatrix0" );																								
		&FreeRegister( \$blendedMatrix1, "\$blendedMatrix1" );																								
		&FreeRegister( \$blendedMatrix2, "\$blendedMatrix2" );																								
	}																								
	elsif( $numBones == 3 )																								
	{																								
		local( $boneIndices );																								
		local( $blendedMatrix0 );																								
		local( $blendedMatrix1 );																								
		local( $blendedMatrix2 );																								
		&AllocateRegister( \$boneIndices, "\$boneIndices" );																								
		&AllocateRegister( \$blendedMatrix0, "\$blendedMatrix0" );																								
		&AllocateRegister( \$blendedMatrix1, "\$blendedMatrix1" );																								
		&AllocateRegister( \$blendedMatrix2, "\$blendedMatrix2" );																								
		push @output, "		; Transform position into world space using all bones																								; LINEINFO(macros.vsh)(903)\n";
		push @output, "		; denormalize d3dcolor to matrix index																								; LINEINFO(macros.vsh)(904)\n";
		push @output, "		mad $boneIndices, $vBoneIndices, $cColorToIntScale, $cModel0Index																								; LINEINFO(macros.vsh)(905)\n";
		push @output, "		; r11 = boneindices at this point																								; LINEINFO(macros.vsh)(906)\n";
		push @output, "		; first matrix																								; LINEINFO(macros.vsh)(907)\n";
		push @output, "		mov a0.x, $boneIndices.z																								; LINEINFO(macros.vsh)(908)\n";
		push @output, "		mul $blendedMatrix0, $vBoneWeights.x, c[a0.x]																								; LINEINFO(macros.vsh)(909)\n";
		push @output, "		mul $blendedMatrix1, $vBoneWeights.x, c[a0.x+1]																								; LINEINFO(macros.vsh)(910)\n";
		push @output, "		mul $blendedMatrix2, $vBoneWeights.x, c[a0.x+2]																								; LINEINFO(macros.vsh)(911)\n";
		push @output, "		; second matrix																								; LINEINFO(macros.vsh)(912)\n";
		push @output, "		mov a0.x, $boneIndices.y																								; LINEINFO(macros.vsh)(913)\n";
		push @output, "		mad $blendedMatrix0, $vBoneWeights.y, c[a0.x], $blendedMatrix0																								; LINEINFO(macros.vsh)(914)\n";
		push @output, "		mad $blendedMatrix1, $vBoneWeights.y, c[a0.x+1], $blendedMatrix1																								; LINEINFO(macros.vsh)(915)\n";
		push @output, "		mad $blendedMatrix2, $vBoneWeights.y, c[a0.x+2], $blendedMatrix2																								; LINEINFO(macros.vsh)(916)\n";
		push @output, "		; Calculate third weight																								; LINEINFO(macros.vsh)(918)\n";
		push @output, "		; compute 1-(weight1+weight2) to calculate weight2																								; LINEINFO(macros.vsh)(919)\n";
		push @output, "		; Use $boneIndices.w as a temp since we aren't using it for anything.																								; LINEINFO(macros.vsh)(920)\n";
		push @output, "		add $boneIndices.w, $vBoneWeights.x, $vBoneWeights.y																								; LINEINFO(macros.vsh)(921)\n";
		push @output, "		sub $boneIndices.w, $cOne, $boneIndices.w																								; LINEINFO(macros.vsh)(922)\n";
		push @output, "		; third matrix																								; LINEINFO(macros.vsh)(924)\n";
		push @output, "		mov a0.x, $boneIndices.x																								; LINEINFO(macros.vsh)(925)\n";
		push @output, "		mad $blendedMatrix0, $boneIndices.w, c[a0.x], $blendedMatrix0																								; LINEINFO(macros.vsh)(926)\n";
		push @output, "		mad $blendedMatrix1, $boneIndices.w, c[a0.x+1], $blendedMatrix1																								; LINEINFO(macros.vsh)(927)\n";
		push @output, "		mad $blendedMatrix2, $boneIndices.w, c[a0.x+2], $blendedMatrix2																								; LINEINFO(macros.vsh)(928)\n";
		push @output, "		; position																								; LINEINFO(macros.vsh)(930)\n";
		push @output, "		dp4 $worldPos.x, $vPos, $blendedMatrix0																								; LINEINFO(macros.vsh)(931)\n";
		push @output, "		dp4 $worldPos.y, $vPos, $blendedMatrix1																								; LINEINFO(macros.vsh)(932)\n";
		push @output, "		dp4 $worldPos.z, $vPos, $blendedMatrix2																								; LINEINFO(macros.vsh)(933)\n";
		push @output, "		mov $worldPos.w, $cOne																								; LINEINFO(macros.vsh)(934)\n";
		push @output, "		; normal																								; LINEINFO(macros.vsh)(936)\n";
		push @output, "		dp3 $worldNormal.x, $vNormal, $blendedMatrix0																								; LINEINFO(macros.vsh)(937)\n";
		push @output, "		dp3 $worldNormal.y, $vNormal, $blendedMatrix1																								; LINEINFO(macros.vsh)(938)\n";
		push @output, "		dp3 $worldNormal.z, $vNormal, $blendedMatrix2																								; LINEINFO(macros.vsh)(939)\n";
		push @output, "		; tangents																								; LINEINFO(macros.vsh)(941)\n";
		push @output, "		dp3 $worldTangentS.x, $vUserData, $blendedMatrix0																								; LINEINFO(macros.vsh)(942)\n";
		push @output, "		dp3 $worldTangentS.y, $vUserData, $blendedMatrix1																								; LINEINFO(macros.vsh)(943)\n";
		push @output, "		dp3 $worldTangentS.z, $vUserData, $blendedMatrix2																								; LINEINFO(macros.vsh)(944)\n";
		push @output, "		; calculate tangent t via cross( N, S ) * S[3]																								; LINEINFO(macros.vsh)(946)\n";
		&Cross( $worldTangentT, $worldNormal, $worldTangentS );																								
		push @output, "		mul $worldTangentT.xyz, $vUserData.w, $worldTangentT.xyz																								; LINEINFO(macros.vsh)(948)\n";
		&FreeRegister( \$boneIndices, "\$boneIndices" );																								
		&FreeRegister( \$blendedMatrix0, "\$blendedMatrix0" );																								
		&FreeRegister( \$blendedMatrix1, "\$blendedMatrix1" );																								
		&FreeRegister( \$blendedMatrix2, "\$blendedMatrix2" );																								
	}																								
}																								
sub ColorClamp																								
{																								
	push @output, "	; ColorClamp; stomps $color.w																								; LINEINFO(macros.vsh)(959)\n";
	local( $color ) = shift;																								
	local( $dst ) = shift;																								
	push @output, "	; Get the max of RGB and stick it in W																								; LINEINFO(macros.vsh)(963)\n";
	push @output, "	max $color.w, $color.x, $color.y																								; LINEINFO(macros.vsh)(964)\n";
	push @output, "	max $color.w, $color.w, $color.z																								; LINEINFO(macros.vsh)(965)\n";
	push @output, "	; get the greater of one and the max color.																								; LINEINFO(macros.vsh)(967)\n";
	push @output, "	max $color.w, $color.w, $cOne																								; LINEINFO(macros.vsh)(968)\n";
	push @output, "	rcp $color.w, $color.w																								; LINEINFO(macros.vsh)(970)\n";
	push @output, "	mul $dst.xyz, $color.w, $color.xyz																								; LINEINFO(macros.vsh)(971)\n";
}																								
sub AmbientLight																								
{																								
	local( $worldNormal ) = shift;																								
	local( $linearColor ) = shift;																								
	local( $add ) = shift;																								
	push @output, "	; Ambient lighting																								; LINEINFO(macros.vsh)(980)\n";
	&AllocateRegister( \$nSquared, "\$nSquared" );																								
	&AllocateRegister( \$isNegative, "\$isNegative" );																								
	push @output, "	mul $nSquared.xyz, $worldNormal.xyz, $worldNormal.xyz				; compute n times n																								; LINEINFO(macros.vsh)(984)\n";
	push @output, "	slt $isNegative.xyz, $worldNormal.xyz, $cZero				; Figure out whether each component is >0																								; LINEINFO(macros.vsh)(985)\n";
	push @output, "	mov a0.x, $isNegative.x																								; LINEINFO(macros.vsh)(986)\n";
	if( $add )																								
	{																								
		push @output, "		mad $linearColor.xyz, $nSquared.x, c[a0.x + $cAmbientColorPosXOffset], $linearColor			; $linearColor = normal[0]*normal[0] * box color of appropriate x side																								; LINEINFO(macros.vsh)(989)\n";
	}																								
	else																								
	{																								
		push @output, "		mul $linearColor.xyz, $nSquared.x, c[a0.x + $cAmbientColorPosXOffset]			; $linearColor = normal[0]*normal[0] * box color of appropriate x side																								; LINEINFO(macros.vsh)(993)\n";
	}																								
	push @output, "	mov a0.x, $isNegative.y																								; LINEINFO(macros.vsh)(995)\n";
	push @output, "	mad $linearColor.xyz, $nSquared.y, c[a0.x + $cAmbientColorPosYOffset], $linearColor																								; LINEINFO(macros.vsh)(996)\n";
	push @output, "	mov a0.x, $isNegative.z																								; LINEINFO(macros.vsh)(997)\n";
	push @output, "	mad $linearColor.xyz, $nSquared.z, c[a0.x + $cAmbientColorPosZOffset], $linearColor																								; LINEINFO(macros.vsh)(998)\n";
	&FreeRegister( \$isNegative, "\$isNegative" );																								
	&FreeRegister( \$nSquared, "\$nSquared" );																								
}																								
sub DirectionalLight																								
{																								
	local( $worldNormal ) = shift;																								
	local( $linearColor ) = shift;																								
	local( $add ) = shift;																								
	&AllocateRegister( \$nDotL, "\$nDotL" ); # FIXME: This only needs to be a scalar																								
	push @output, "	; NOTE: Gotta use -l here, since light direction = -l																								; LINEINFO(macros.vsh)(1012)\n";
	push @output, "	; DIRECTIONAL LIGHT																								; LINEINFO(macros.vsh)(1013)\n";
	push @output, "	; compute n dot l																								; LINEINFO(macros.vsh)(1014)\n";
	push @output, "	dp3 $nDotL.x, -c[a0.x + 1], $worldNormal																								; LINEINFO(macros.vsh)(1015)\n";
	push @output, "	max $nDotL.x, $nDotL.x, c0.x			; Clamp to zero																								; LINEINFO(macros.vsh)(1016)\n";
	if( $add )																								
	{																								
		push @output, "		mad $linearColor.xyz, c[a0.x], $nDotL.x, $linearColor																								; LINEINFO(macros.vsh)(1019)\n";
	}																								
	else																								
	{																								
		push @output, "		mov $linearColor.xyz, c[a0.x], $nDotL.x																								; LINEINFO(macros.vsh)(1023)\n";
	}																								
	&FreeRegister( \$nDotL, "\$nDotL" );																								
}																								
sub PointLight																								
{																								
	local( $worldPos ) = shift;																								
	local( $worldNormal ) = shift;																								
	local( $linearColor ) = shift;																								
	local( $add ) = shift;																								
	local( $lightDir );																								
	&AllocateRegister( \$lightDir, "\$lightDir" );																								
	push @output, "	; POINT LIGHT																								; LINEINFO(macros.vsh)(1039)\n";
	push @output, "	; compute light direction																								; LINEINFO(macros.vsh)(1040)\n";
	push @output, "	sub $lightDir, c[a0.x+2], $worldPos																								; LINEINFO(macros.vsh)(1041)\n";
	local( $lightDistSquared );																								
	local( $ooLightDist );																								
	&AllocateRegister( \$lightDistSquared, "\$lightDistSquared" );																								
	&AllocateRegister( \$ooLightDist, "\$ooLightDist" );																								
	push @output, "	; normalize light direction, maintain temporaries for attenuation																								; LINEINFO(macros.vsh)(1048)\n";
	push @output, "	dp3 $lightDistSquared, $lightDir, $lightDir																								; LINEINFO(macros.vsh)(1049)\n";
	push @output, "	rsq $ooLightDist, $lightDistSquared.x																								; LINEINFO(macros.vsh)(1050)\n";
	push @output, "	mul $lightDir, $lightDir, $ooLightDist.x																								; LINEINFO(macros.vsh)(1051)\n";
	local( $attenuationFactors );																								
	&AllocateRegister( \$attenuationFactors, "\$attenuationFactors" );																								
	push @output, "	; compute attenuation amount (r2 = 'd*d d*d d*d d*d', r3 = '1/d 1/d 1/d 1/d')																								; LINEINFO(macros.vsh)(1056)\n";
	push @output, "	dst $attenuationFactors, $lightDistSquared, $ooLightDist						; r4 = ( 1, d, d*d, 1/d )																								; LINEINFO(macros.vsh)(1057)\n";
	&FreeRegister( \$lightDistSquared, "\$lightDistSquared" );																								
	&FreeRegister( \$ooLightDist, "\$ooLightDist" );																								
	local( $attenuation );																								
	&AllocateRegister( \$attenuation, "\$attenuation" );																								
	push @output, "	dp3 $attenuation, $attenuationFactors, c[a0.x+4]				; r3 = atten0 + d * atten1 + d*d * atten2																								; LINEINFO(macros.vsh)(1062)\n";
	push @output, "	rcp $lightDir.w, $attenuation						; $lightDir.w = 1 / (atten0 + d * atten1 + d*d * atten2)																								; LINEINFO(macros.vsh)(1064)\n";
	&FreeRegister( \$attenuationFactors, "\$attenuationFactors" );																								
	&FreeRegister( \$attenuation, "\$attenuation" );																								
	local( $tmp );																								
	&AllocateRegister( \$tmp, "\$tmp" ); # FIXME : really only needs to be a scalar																								
	push @output, "	; compute n dot l, fold in distance attenutation																								; LINEINFO(macros.vsh)(1072)\n";
	push @output, "	dp3 $tmp.x, $lightDir, $worldNormal																								; LINEINFO(macros.vsh)(1073)\n";
	push @output, "	max $tmp.x, $tmp.x, c0.x				; Clamp to zero																								; LINEINFO(macros.vsh)(1074)\n";
	push @output, "	mul $tmp.x, $tmp.x, $lightDir.w																								; LINEINFO(macros.vsh)(1075)\n";
	if( $add )																								
	{																								
		push @output, "		mad $linearColor.xyz, c[a0.x], $tmp.x, $linearColor																								; LINEINFO(macros.vsh)(1078)\n";
	}																								
	else																								
	{																								
		push @output, "		mov $linearColor.xyz, c[a0.x], $tmp.x																								; LINEINFO(macros.vsh)(1082)\n";
	}																								
	&FreeRegister( \$lightDir, "\$lightDir" );																								
	&FreeRegister( \$tmp, "\$tmp" ); # FIXME : really only needs to be a scalar																								
}																								
sub SpotLight																								
{																								
	local( $worldPos ) = shift;																								
	local( $worldNormal ) = shift;																								
	local( $linearColor ) = shift;																								
	local( $add ) = shift;																								
	local( $lightDir );																								
	&AllocateRegister( \$lightDir, "\$lightDir" );																								
	push @output, "	; SPOTLIGHT																								; LINEINFO(macros.vsh)(1099)\n";
	push @output, "	; compute light direction																								; LINEINFO(macros.vsh)(1100)\n";
	push @output, "	sub $lightDir, c[a0.x+2], $worldPos																								; LINEINFO(macros.vsh)(1101)\n";
	local( $lightDistSquared );																								
	local( $ooLightDist );																								
	&AllocateRegister( \$lightDistSquared, "\$lightDistSquared" );																								
	&AllocateRegister( \$ooLightDist, "\$ooLightDist" );																								
	push @output, "	; normalize light direction, maintain temporaries for attenuation																								; LINEINFO(macros.vsh)(1108)\n";
	push @output, "	dp3 $lightDistSquared, $lightDir, $lightDir																								; LINEINFO(macros.vsh)(1109)\n";
	push @output, "	rsq $ooLightDist, $lightDistSquared.x																								; LINEINFO(macros.vsh)(1110)\n";
	push @output, "	mul $lightDir, $lightDir, $ooLightDist.x																								; LINEINFO(macros.vsh)(1111)\n";
	local( $attenuationFactors );																								
	&AllocateRegister( \$attenuationFactors, "\$attenuationFactors" );																								
	push @output, "	; compute attenuation amount (r2 = 'd*d d*d d*d d*d', r3 = '1/d 1/d 1/d 1/d')																								; LINEINFO(macros.vsh)(1116)\n";
	push @output, "	dst $attenuationFactors, $lightDistSquared, $ooLightDist						; r4 = ( 1, d, d*d, 1/d )																								; LINEINFO(macros.vsh)(1117)\n";
	&FreeRegister( \$lightDistSquared, "\$lightDistSquared" );																								
	&FreeRegister( \$ooLightDist, "\$ooLightDist" );																								
	local( $attenuation );	&AllocateRegister( \$attenuation, "\$attenuation" );																								
	push @output, "	dp3 $attenuation, $attenuationFactors, c[a0.x+4]				; r3 = atten0 + d * atten1 + d*d * atten2																								; LINEINFO(macros.vsh)(1123)\n";
	push @output, "	rcp $lightDir.w, $attenuation						; r1.w = 1 / (atten0 + d * atten1 + d*d * atten2)																								; LINEINFO(macros.vsh)(1124)\n";
	&FreeRegister( \$attenuationFactors, "\$attenuationFactors" );																								
	&FreeRegister( \$attenuation, "\$attenuation" );																								
	local( $litSrc ); &AllocateRegister( \$litSrc, "\$litSrc" );																								
	local( $tmp ); &AllocateRegister( \$tmp, "\$tmp" ); # FIXME - only needs to be scalar																								
	push @output, "	; compute n dot l																								; LINEINFO(macros.vsh)(1132)\n";
	push @output, "	dp3 $litSrc.x, $worldNormal, $lightDir																								; LINEINFO(macros.vsh)(1133)\n";
	push @output, "	; compute angular attenuation																								; LINEINFO(macros.vsh)(1135)\n";
	push @output, "	dp3 $tmp.x, c[a0.x+1], -$lightDir				; dot = -delta * spot direction																								; LINEINFO(macros.vsh)(1136)\n";
	push @output, "	sub $litSrc.y, $tmp.x, c[a0.x+3].z				; r2.y = dot - stopdot2																								; LINEINFO(macros.vsh)(1137)\n";
	&FreeRegister( \$tmp, "\$tmp" );																								
	push @output, "	mul $litSrc.y, $litSrc.y, c[a0.x+3].w			; r2.y = (dot - stopdot2) / (stopdot - stopdot2)																								; LINEINFO(macros.vsh)(1139)\n";
	push @output, "	mov $litSrc.w, c[a0.x+3].x						; r2.w = exponent																								; LINEINFO(macros.vsh)(1140)\n";
	local( $litDst ); &AllocateRegister( \$litDst, "\$litDst" );																								
	push @output, "	lit $litDst, $litSrc							; r3.y = N dot L or 0, whichever is bigger																								; LINEINFO(macros.vsh)(1142)\n";
	&FreeRegister( \$litSrc, "\$litSrc" );																								
													push @output, "													; r3.z = pow((dot - stopdot2) / (stopdot - stopdot2), exponent)																								; LINEINFO(macros.vsh)(1144)\n";
	push @output, "	min $litDst.z, $litDst.z, $cOne		 			; clamp pow() to 1																								; LINEINFO(macros.vsh)(1145)\n";
	local( $tmp1 ); &AllocateRegister( \$tmp1, "\$tmp1" );																								
	local( $tmp2 ); &AllocateRegister( \$tmp2, "\$tmp2" );  # FIXME - could be scalar																								
	push @output, "	; fold in distance attenutation with other factors																								; LINEINFO(macros.vsh)(1150)\n";
	push @output, "	mul $tmp1, c[a0.x], $lightDir.w																								; LINEINFO(macros.vsh)(1151)\n";
	push @output, "	mul $tmp2.x, $litDst.y, $litDst.z																								; LINEINFO(macros.vsh)(1152)\n";
	if( $add )																								
	{																								
		push @output, "		mad $linearColor.xyz, $tmp1, $tmp2.x, $linearColor																								; LINEINFO(macros.vsh)(1155)\n";
	}																								
	else																								
	{																								
		push @output, "		mov $linearColor.xyz, $tmp1, $tmp2.x																								; LINEINFO(macros.vsh)(1159)\n";
	}																								
	&FreeRegister( \$lightDir, "\$lightDir" );																								
	&FreeRegister( \$litDst, "\$litDst" );																								
	&FreeRegister( \$tmp1, "\$tmp1" );																								
	&FreeRegister( \$tmp2, "\$tmp2" );																								
}																								
sub DoLight																								
{																								
	local( $lightType ) = shift;																								
	local( $worldPos ) = shift;																								
	local( $worldNormal ) = shift;																								
	local( $linearColor ) = shift;																								
	local( $add ) = shift;																								
	if( $lightType eq "spot" )																								
	{																								
		&SpotLight( $worldPos, $worldNormal, $linearColor, $add );																								
	}																								
	elsif( $lightType eq "point" )																								
	{																								
		&PointLight( $worldPos, $worldNormal, $linearColor, $add );																								
	}																								
	elsif( $lightType eq "directional" )																								
	{																								
		&DirectionalLight( $worldNormal, $linearColor, $add );																								
	}																								
	else																								
	{																								
		die "don't know about light type \"$lightType\"\n";																								
	}																								
}																								
sub DoLighting																								
{																								
	local( $staticLightType ) = shift;																								
	local( $ambientLightType ) = shift;																								
	local( $localLightType1 ) = shift;																								
	local( $localLightType2 ) = shift;																								
	local( $worldPos ) = shift;																								
	local( $worldNormal ) = shift;																								
	# special case for no lighting																								
	if( $staticLightType eq "none" && $ambientLightType eq "none" &&																								
		$localLightType1 eq "none" && $localLightType2 eq "none" )																								
	{																								
		return;																								
	}																								
	# special case for static lighting only																								
	# Don't need to bother converting to linear space in this case.																								
	if( $staticLightType eq "static" && $ambientLightType eq "none" &&																								
		$localLightType1 eq "none" && $localLightType2 eq "none" )																								
	{																								
		push @output, "		mov oD0, $vSpecular																								; LINEINFO(macros.vsh)(1215)\n";
		return;																								
	}																								
	local( $linearColor ); &AllocateRegister( \$linearColor, "\$linearColor" );
	local( $gammaColor ); &AllocateRegister( \$gammaColor, "\$gammaColor" );
	local( $add ) = 0;																								
	if( $staticLightType eq "static" )																								
	{																								
		push @output, "		; The static lighting comes in in gamma space and has also been premultiplied by $cOverbrightFactor																								; LINEINFO(macros.vsh)(1225)\n";
		push @output, "		; need to get it into																								; LINEINFO(macros.vsh)(1226)\n";
		push @output, "		; linear space so that we can do adds.																								; LINEINFO(macros.vsh)(1227)\n";
		push @output, "		rcp $gammaColor.w, $cOverbrightFactor																								; LINEINFO(macros.vsh)(1228)\n";
		push @output, "		mul $gammaColor.xyz, $vSpecular, $gammaColor.w																								; LINEINFO(macros.vsh)(1229)\n";
		&GammaToLinear( $gammaColor, $linearColor );																								
		$add = 1;																								
	}																								
	if( $ambientLightType eq "ambient" )																								
	{																								
		&AmbientLight( $worldNormal, $linearColor, $add );																								
		$add = 1;																								
	}																								
	if( $localLightType1 ne "none" )																								
	{																								
		push @output, "		mov a0.x, c3.x																								; LINEINFO(macros.vsh)(1242)\n";
		&DoLight( $localLightType1, $worldPos, $worldNormal, $linearColor, $add );																								
		$add = 1;																								
	}																								
	if( $localLightType2 ne "none" )																								
	{																								
		push @output, "		mov a0.x, c3.y																								; LINEINFO(macros.vsh)(1249)\n";
		&DoLight( $localLightType2, $worldPos, $worldNormal, $linearColor, $add );																								
		$add = 1;																								
	}																								
	push @output, "	;------------------------------------------------------------------------------																								; LINEINFO(macros.vsh)(1254)\n";
	push @output, "	; Output color (gamma correction)																								; LINEINFO(macros.vsh)(1255)\n";
	push @output, "	;------------------------------------------------------------------------------																								; LINEINFO(macros.vsh)(1256)\n";
	&LinearToGamma( $linearColor, $gammaColor );																								
	if( 0 )																								
	{																								
		push @output, "		mul oD0.xyz, $gammaColor.xyz, $cOverbrightFactor																								; LINEINFO(macros.vsh)(1261)\n";
	}																								
	else																								
	{																								
		push @output, "		mul $gammaColor.xyz, $gammaColor.xyz, $cOverbrightFactor																								; LINEINFO(macros.vsh)(1265)\n";
		&ColorClamp( $gammaColor, "oD0" );																								
	}																								
 	push @output, ";	mov oD0.xyz, $linearColor																								; LINEINFO(macros.vsh)(1269)\n";
	push @output, "	mov oD0.w, c0.y				; make sure all components are defined																								; LINEINFO(macros.vsh)(1270)\n";
	&FreeRegister( \$linearColor, "\$linearColor" );
	&FreeRegister( \$gammaColor, "\$gammaColor" );
}																								
sub DoDynamicLightingToLinear																								
{																								
	local( $ambientLightType ) = shift;																								
	local( $localLightType1 ) = shift;																								
	local( $localLightType2 ) = shift;																								
	local( $worldPos ) = shift;																								
	local( $worldNormal ) = shift;																								
	local( $linearColor ) = shift;																								
	# No lights at all. . note that we don't even consider static lighting here.																								
	if( $ambientLightType eq "none" &&																								
		$localLightType1 eq "none" && $localLightType2 eq "none" )																								
	{																								
		push @output, "		mov $linearColor, $cZero																								; LINEINFO(macros.vsh)(1289)\n";
		return;																								
	}																								
	local( $add ) = 0;																								
	if( $ambientLightType eq "ambient" )																								
	{																								
		&AmbientLight( $worldNormal, $linearColor, $add );																								
		$add = 1;																								
	}																								
	if( $localLightType1 ne "none" )																								
	{																								
		push @output, "		mov a0.x, c3.x																								; LINEINFO(macros.vsh)(1302)\n";
		&DoLight( $localLightType1, $worldPos, $worldNormal, $linearColor, $add );																								
		$add = 1;																								
	}																								
	if( $localLightType2 ne "none" )																								
	{																								
		push @output, "		mov a0.x, c3.y																								; LINEINFO(macros.vsh)(1309)\n";
		&DoLight( $localLightType2, $worldPos, $worldNormal, $linearColor, $add );																								
		$add = 1;																								
	}																								
}																								
push @output, ";------------------------------------------------------------------------------																								; LINEINFO(Jojirium.vsh)(10)\n";
push @output, "; Vertex blending																								; LINEINFO(Jojirium.vsh)(11)\n";
push @output, ";------------------------------------------------------------------------------																								; LINEINFO(Jojirium.vsh)(12)\n";
&AllocateRegister( \$worldPos, "\$worldPos" );																								
push @output, "; Transform position from object to world																								; LINEINFO(Jojirium.vsh)(16)\n";
push @output, "dp4 $worldPos.x, $vPos, $cModel0																								; LINEINFO(Jojirium.vsh)(17)\n";
push @output, "dp4 $worldPos.y, $vPos, $cModel1																								; LINEINFO(Jojirium.vsh)(18)\n";
push @output, "dp4 $worldPos.z, $vPos, $cModel2																								; LINEINFO(Jojirium.vsh)(19)\n";
&AllocateRegister( \$projPos, "\$projPos" );																								
push @output, "; Transform position from object to projection space																								; LINEINFO(Jojirium.vsh)(23)\n";
push @output, "dp4 $projPos.x, $vPos, $cModelViewProj0																								; LINEINFO(Jojirium.vsh)(24)\n";
push @output, "dp4 $projPos.y, $vPos, $cModelViewProj1																								; LINEINFO(Jojirium.vsh)(25)\n";
push @output, "dp4 $projPos.z, $vPos, $cModelViewProj2																								; LINEINFO(Jojirium.vsh)(26)\n";
push @output, "dp4 $projPos.w, $vPos, $cModelViewProj3																								; LINEINFO(Jojirium.vsh)(27)\n";
push @output, "mov oPos, $projPos																								; LINEINFO(Jojirium.vsh)(29)\n";
push @output, ";------------------------------------------------------------------------------																								; LINEINFO(Jojirium.vsh)(31)\n";
push @output, "; Lighting																								; LINEINFO(Jojirium.vsh)(32)\n";
push @output, ";------------------------------------------------------------------------------																								; LINEINFO(Jojirium.vsh)(33)\n";
push @output, "; Transform tangent space basis vectors to env map space (world space)																								; LINEINFO(Jojirium.vsh)(35)\n";
push @output, "; This will produce a set of vectors mapping from tangent space to env space																								; LINEINFO(Jojirium.vsh)(36)\n";
push @output, "; We'll use this to transform normals from the normal map from tangent space																								; LINEINFO(Jojirium.vsh)(37)\n";
push @output, "; to environment map space. 																								; LINEINFO(Jojirium.vsh)(38)\n";
push @output, "; NOTE: use dp3 here since the basis vectors are vectors, not points																								; LINEINFO(Jojirium.vsh)(39)\n";
push @output, "dp3 oT1.x, $vTangentS, c90																								; LINEINFO(Jojirium.vsh)(41)\n";
push @output, "dp3 oT2.x, $vTangentS, c91																								; LINEINFO(Jojirium.vsh)(42)\n";
push @output, "dp3 oT3.x, $vTangentS, c92																								; LINEINFO(Jojirium.vsh)(43)\n";
push @output, "dp3 oT1.y, $vTangentT, c90																								; LINEINFO(Jojirium.vsh)(45)\n";
push @output, "dp3 oT2.y, $vTangentT, c91																								; LINEINFO(Jojirium.vsh)(46)\n";
push @output, "dp3 oT3.y, $vTangentT, c92																								; LINEINFO(Jojirium.vsh)(47)\n";
push @output, "dp3 oT1.z, $vNormal, c90																								; LINEINFO(Jojirium.vsh)(49)\n";
push @output, "dp3 oT2.z, $vNormal, c91																								; LINEINFO(Jojirium.vsh)(50)\n";
push @output, "dp3 oT3.z, $vNormal, c92																								; LINEINFO(Jojirium.vsh)(51)\n";
&AllocateRegister( \$worldToEye, "\$worldToEye" );																								
push @output, "; Compute the vector from vertex to camera																								; LINEINFO(Jojirium.vsh)(55)\n";
push @output, "sub $worldToEye.xyz, $cEyePos, $worldPos																								; LINEINFO(Jojirium.vsh)(56)\n";
push @output, ";------------------------------------------------------------------------------																								; LINEINFO(Jojirium.vsh)(58)\n";
push @output, "; Fog																								; LINEINFO(Jojirium.vsh)(59)\n";
push @output, ";------------------------------------------------------------------------------																								; LINEINFO(Jojirium.vsh)(60)\n";
&CalcFog( $worldPos, $projPos );																								
&FreeRegister( \$worldPos, "\$worldPos" );																								
push @output, "; Move it into the w component of the texture coords, as the wacky																								; LINEINFO(Jojirium.vsh)(66)\n";
push @output, "; pixel shader wants it there.																								; LINEINFO(Jojirium.vsh)(67)\n";
push @output, "mov oT1.w, $worldToEye.x																								; LINEINFO(Jojirium.vsh)(68)\n";
push @output, "mov oT2.w, $worldToEye.y																								; LINEINFO(Jojirium.vsh)(69)\n";
push @output, "mov oT3.w, $worldToEye.z																								; LINEINFO(Jojirium.vsh)(70)\n";
&FreeRegister( \$worldToEye, "\$worldToEye" );																								
push @output, ";------------------------------------------------------------------------------																								; LINEINFO(Jojirium.vsh)(74)\n";
push @output, "; Texture coordinates (normal map)																								; LINEINFO(Jojirium.vsh)(75)\n";
push @output, ";------------------------------------------------------------------------------																								; LINEINFO(Jojirium.vsh)(76)\n";
push @output, "dp4 oT0.x, $vTexCoord0, c94																								; LINEINFO(Jojirium.vsh)(77)\n";
push @output, "dp4 oT0.y, $vTexCoord0, c95																								; LINEINFO(Jojirium.vsh)(78)\n";
&FreeRegister( \$projPos, "\$projPos" );																								
