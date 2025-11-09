#!/usr/bin/env -S python3 -B
# -*- coding: utf-8 -*-

# Copyright (c) 2018 - 2021 Mario Mlačak, mmlacak@gmail.com
# Licensed under GNU GPL v3+ license. See LICENSING, COPYING files for details.


# from scene import Scene
from scene_mixin import SceneMixin
from scene_classical_chess import SceneClassicalChessMixin
from scene_croatian_ties import SceneCroatianTiesMixin
from scene_mayan_ascendancy import SceneMayanAscendancyMixin
from scene_age_of_aquarius import SceneAgeOfAquariusMixin
from scene_mirandas_veil import SceneMirandasVeilMixin
from scene_nineteen import SceneNineteenMixin
from scene_hemeras_dawn import SceneHemerasDawnMixin
from scene_tamoanchan_revisited import SceneTamoanchanRevisitedMixin
from scene_conquest_of_tlalocan import SceneConquestOfTlalocanMixin
from scene_discovery import SceneDiscoveryMixin
from scene_one import SceneOneMixin
from scene_simple import SceneSimpleMixin


class SceneMix( SceneMixin, \
                SceneClassicalChessMixin, \
                SceneCroatianTiesMixin, \
                SceneMayanAscendancyMixin, \
                SceneAgeOfAquariusMixin, \
                SceneMirandasVeilMixin, \
                SceneNineteenMixin, \
                SceneHemerasDawnMixin, \
                SceneTamoanchanRevisitedMixin, \
                SceneConquestOfTlalocanMixin, \
                SceneDiscoveryMixin, \
                SceneOneMixin, \
                SceneSimpleMixin ):

    # overrides
    def _get_recent_scene_method_names( self ):
        return  [
                    # 'scn_hd_01_centaur_same_color', \
                    # 'scn_hd_02_centaur_opposite_color', \
                    # 'scn_hd_15_scout_movement', \
                    # 'scn_hd_16_scout_capturing', \
                    # 'scn_hd_17_scout_forking_steps', \
                    # 'scn_hd_20_scout_rerouting', \
                    # 'scn_hd_21_scout_rerouting_first_step', \
                    # 'scn_hd_22_scout_rerouting_pawn_wall', \
                    # 'scn_hd_30_scout_en_passant', \
                    # 'scn_hd_40_grenadier_fields', \
                    # 'scn_hd_42_grenadier_movement_transition', \
                    # 'scn_hd_43_grenadier_forking_steps', \
                    # 'scn_hd_44_grenadier_vertical_steps', \
                    # 'scn_hd_45_grenadier_horizontal_steps', \
                    # 'scn_hd_46_grenadier_close_quarters_transition', \
                    # 'scn_hd_47_grenadier_blocked_steps', \
                    # 'scn_hd_48_grenadier_not_blocked_steps', \
                    # 'scn_hd_59_grenadier_en_passant', \
                    # 'scn_hd_60_grenadier_en_passant_self_extended', \

                    # 'scn_tr_01_serpent_diagonals', \
                    # 'scn_tr_02_serpent_1', \
                    # 'scn_tr_03_serpent_2', \
                    # 'scn_tr_04_serpent_3', \
                    # 'scn_tr_05_serpent_end', \
                    # 'scn_tr_06_serpent_loop_illegal', \

                    # 'scn_tr_30_ritual_starting_fields', \

                    # 'scn_cot_003_light_shaman_step_ply_no_capture', \
                    # 'scn_cot_044_king_cannot_diverge', \
                    # 'scn_cot_045_diverging_stepping_shaman', \
                    # 'scn_cot_046_diverging_capturing_shaman', \
                    # 'scn_cot_049_diverging_activated_shaman', \
                    # 'scn_cot_050_cannot_diverge_activated_shaman', \
                    # 'scn_cot_070_trance_fields', \
                    # 'scn_cot_071_entrancement_init', \
                    # 'scn_cot_072_entrancement_step', \
                    # 'scn_cot_073_entrancement_activated', \
                    # 'scn_cot_074_entrancement_repositioning', \
                    # 'scn_cot_075_entrancement_cascade', \
                    # 'scn_cot_076_knight_directions', \
                    # 'scn_cot_077_stop_sign_pattern', \
                    # 'scn_cot_080_stop_sign_pattern_unwind', \

                    # 'scn_d_10_monolith_rerouting_scout', \

                    # 'scn_o_01_starchild_movement', \
                    # 'scn_o_02_starchild_activating_own_starchild', \
                    # 'scn_o_03_starchild_activating_own_wave', \
                    # 'scn_o_05_miracle_fields', \
                    # 'scn_o_09_starchild_moving_star_init', \
                    # 'scn_o_10_starchild_moving_star_end', \
                    # 'scn_o_11_starchild_moving_star_activating', \
                    # 'scn_o_12_star_movement_blocked_init', \
                    # 'scn_o_13_star_movement_blocked_end', \
                    # 'scn_o_14_star_rerouting_scout', \
                    # 'scn_o_20_starchild_activated_wave_not_teleporting_init', \
                    # 'scn_o_21_starchild_activated_wave_not_teleporting_end', \
                    # 'scn_o_32_activating_piece_surplus_momentum', \
                    # 'scn_o_33_diverging_piece_surplus_momentum', \
                    # 'scn_o_34_starchild_cannot_diverge', \
                    # 'scn_o_35_wave_cannot_diverge', \
                    # 'scn_o_40_uplifting_fields', \
                    # 'scn_o_41_uplifting_init', \
                    # 'scn_o_42_uplifting_step', \
                    # 'scn_o_43_uplifting_activated', \
                    # 'scn_o_44_shaman_initiated_uplifting', \
                    'scn_o_60_king_castling_not_blocked', \
                    'scn_o_61_rook_castling_not_blocked', \
                    'scn_o_62_castling_blocked', \
                ]
